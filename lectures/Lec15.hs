module Lec15 where

import Prelude hiding (Traversable (..))

{- LECTURE 15 : STATE AND TRAVERSABLES -}

{--------------------------------------------------------------------}
{- Part 1. Mutable state                                            -}
{--------------------------------------------------------------------}

newtype State s a = MkState (s -> (a,s))

instance Monad (State s) where
  -- return :: a -> State s a
  return x = MkState (\s -> (x,s))

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  MkState tr1 >>= f =
    MkState (\s1 -> let (a,s2)      = tr1 s1
                        MkState tr2 = f a
                        (b,s3)      = tr2 s2
                    in
                      (b,s3))

instance Functor (State s) where
  fmap f s = pure f <*> s

instance Applicative (State s) where
  pure = return
  tr_f <*> tr_a = do f <- tr_f; a <- tr_a; return (f a)

-- The basic interface for 'State s':

get :: State s s
get = MkState (\s -> (s,s))


put :: s -> State s ()
put s = MkState (\_ -> ((), s))



runState :: State s a -> s -> a
runState (MkState tr) initial =
  let (a, finalState) = tr initial
  in a




{--------------------------------------------------------------------}
{- Part 2. Traversing Trees and Lists                               -}
{--------------------------------------------------------------------}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- Iterating over trees and lists

addUpTree :: Tree Int -> Int
addUpTree tree = runState (do traverseTree tree; get) 0
  where traverseTree :: Tree Int -> State Int ()
        traverseTree Leaf = return ()
        traverseTree (Node l x r) = do
          traverseTree l
          currentTotal <- get
          put (currentTotal + x)
          traverseTree r


addUpList :: [Int] -> Int
addUpList xs = runState (do traverseList xs; get) 0
  where traverseList [] = return ()
        traverseList (x:xs) = do
          currentTotal <- get
          put (currentTotal + x)
          traverseList xs








-- Bob: Can we generalise over the container type?

instance Foldable Tree where
  --foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap measure Leaf = mempty
  foldMap measure (Node l x r) =
    foldMap measure l
    `mappend` measure x
    `mappend` foldMap measure r

-- Actions form a monoid

nothing :: Applicative f => f ()
nothing = pure ()

andThen :: Applicative f => f () -> f () -> f ()
andThen x y = (\() () -> ()) <$> x <*> y

newtype ApplMonoid f = MkApplMonoid (f ())


instance Applicative f => Monoid (ApplMonoid f) where
  mempty = MkApplMonoid (pure ())
  MkApplMonoid x `mappend` MkApplMonoid y = MkApplMonoid ((\() () -> ()) <$> x <*> y)


traverse_ :: (Foldable f, Applicative g) =>
             (a -> g ()) -> f a -> g ()
traverse_ measure c =
  let MkApplMonoid a = foldMap (MkApplMonoid . measure) c
  in a


addUp :: (Foldable f, Functor f) => f Int -> State Int ()
addUp = traverse_ (\x -> do currentTotal <- get; put (currentTotal + x))

-- runState (do addUp [1,2,3]; get) 0

{--------------------------------------------------------------------}

-- Fred: how else can we iterate an action over trees?

numberTree :: Tree a -> Tree Int
numberTree tree = runState (traverseTree tree) 0
  where
    traverseTree :: Tree a -> State Int (Tree Int)
    traverseTree Leaf = return Leaf
    traverseTree (Node l _ r) = do
      l' <- traverseTree l
      x' <- get
      put (x' + 1)
      r' <- traverseTree r
      return (Node l' x' r')

traverseTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
traverseTreeM action Leaf = return Leaf
traverseTreeM action (Node l x r) = do
  l' <- traverseTreeM action l
  x' <- action x
  r' <- traverseTreeM action r
  return (Node l' x' r')

numberTree' :: Tree a -> Tree Int
numberTree' t = runState (traverseTreeM (\ a -> do x' <- get; put (x' + 1); return x') t) 0 -- a -> State Int (Tree Int)


traverseTreeA :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverseTreeA action Leaf = pure Leaf
traverseTreeA action (Node l x r) = Node <$> traverseTreeA action l <*>  action x  <*> traverseTreeA action r

