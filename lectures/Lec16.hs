module Lec16 where

import Data.Monoid

{-
   LECTURE 16 : VISITING CONTAINERS AND BUILDING MONADS
-}

{--------------------------------------------------------------------}
{- Part 1. Visiting elements of containers                          -}
{--------------------------------------------------------------------}

{- A problem:

     - We have a container full of values 'box :: c a'

     - We have a checking function 'f :: a -> Maybe b'

     - We want to
         (a) find out if all the 'a's in 'box' are OK
         (b) if so, make a new version of 'box' of type 'c b'
-}

{- A container type: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Visiting pattern 1 : Functor

   A functor applies a function to every element in the container, and
   creates a new container with the same shape, but new elements. -}
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- Example: -}
convertAll :: Functor c => (a -> Maybe b) -> c a -> c (Maybe b)
convertAll checker box = fmap checker box

{- Visiting pattern 2 : Foldable

   A foldable applies a function to every element in the container,
   and combines all the results. -}
instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r

{-  newtype All = All { getAll :: Bool }

    instance Monoid All where
      mempty = All True
      All p `mappend` All q = All (p && q)
-}

checkAll :: Foldable c => (a -> Maybe b) -> c a -> Bool
checkAll checker box = getAll (foldMap checker2 box) where
  -- checker2 :: a -> All
  checker2 x = case checker x of
                 Nothing -> All False
                 Just b  -> All True
  
{- Can we solve our problem now? -}

checkThenConvert :: (Functor c, Foldable c) =>
                    (a -> Maybe b) ->
                    c a ->
                    Maybe (c b)
checkThenConvert checker box =
  if checkAll checker box then
    Just $ fmap (fromJust . checker) box
  else
    Nothing

fromJust :: Maybe b -> b
fromJust (Just x) = x

{- Visiting pattern 3 : Traversable

   A traversable applies a function to every element in the container,
   and (i) creates a new container with the same shape and new
   elements; and (ii) combines results on the side. -}

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Leaf         = pure Leaf
  traverse f (Node l x r) = Node <$> traverse f l
                                 <*> f x
                                 <*> traverse f r

convertAndCheckAll :: Traversable c =>
                      (a -> Maybe b)
                   -> c a
                   -> Maybe (c b)
convertAndCheckAll checker box = traverse checker box



{-
   fmap     ::                  (a -> b)   -> c a  -> c b
   foldMap  :: Monoid m      => (a -> m)   -> c a  -> m
   traverse :: Applicative f => (a -> f b) -> c a  -> f (c b)
-}



{--------------------------------------------------------------------}
{- Part 2. Grow your own Monad                                      -}
{--------------------------------------------------------------------}

{- The reason we are so interested in Monads and Applicatives is that we
   can use them to build our own ideas of how to 'combine' results and
   return values that suit the application we have in mind.

   In this example, we will bulid a Monad/Applicative that acts like
   Maybe in that it can signal failure, but also records a list of log
   messages. This will allow us to traverse a container checking its
   elements, while also logging the elements that we have checked. -}

newtype LogAndFail a = MkLogAndFail ([String], Maybe a)

logMsg :: String -> LogAndFail ()
logMsg msg = MkLogAndFail ([msg], Just ())

failure :: LogAndFail a
failure = MkLogAndFail ([], Nothing)

convertCheckAndLog :: (Traversable c, Show a) =>
                      (a -> Maybe b) ->
                      c a ->
                      LogAndFail (c b)
convertCheckAndLog checker box = traverse loggingChecker box
  where
    -- loggingChecker :: a -> LogAndFail b
    loggingChecker a = do
      logMsg (show a)
      case checker a of
        Nothing -> failure
        Just b  -> return b

{-  'loggingChecker' could have also been written just using the
    Applicative interface:

       loggingChecker a =
         (\_ b -> b) <$> logMsg (show a) <*> case checker a of Nothing -> failure; Just b -> pure b
-}

{- Or we could have written a combined 'logAndReturn' function that logs
   a message and returns a result:

       logAndReturn :: String -> Maybe a -> LogAndFail a
       logAndReturn msg result = MkLogAndFail ([msg], result)

   However, using the separate 'logMsg' and 'failure' functions, and
   the Monad interface gives us more flexibility. For instance, we
   could have written 'loggingChecker' as follows:

       loggingChecker a =
         case checker a of
           Nothing -> do logMsg (show a); failure
           Just b  -> return b

   So we only log the values of the 'a's that fail. -}


instance Monad LogAndFail where
  -- return :: a -> LogAndFail a
  return a = MkLogAndFail ([], Just a)

  -- (>>=) :: LogAndFail a -> (a -> LogAndFail b) -> LogAndFail b
  MkLogAndFail (log1, ma) >>= f =
    case ma of
      Nothing -> MkLogAndFail (log1, Nothing)
      Just a ->
        let MkLogAndFail (log2, mb)= f a
        in
          MkLogAndFail (log1 ++ log2, mb)

instance Functor LogAndFail where
  -- fmap :: (a -> b) -> LogAndFail a -> LogAndFail b
  fmap f lf = do a <- lf; return (f a)

instance Applicative LogAndFail where
  -- pure :: a -> LogAndFail a
  pure  = return

  -- (<*>) :: LogAndFail (a -> b) -> LogAndFail a -> LogAndFail b
  lf_f <*> lf_a = do f <- lf_f
                     a <- lf_a
                     return (f a)
