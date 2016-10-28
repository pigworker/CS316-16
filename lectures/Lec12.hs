module Lec12 where

{- LECTURE 12: Monads -}

import Prelude (Functor(..),Show(..), Either(..), Int, Bool(..), (+), (++),undefined,error,Applicative(..),div,id,(.),Monad(..),flip,(=<<),(<$>))

{- Today

Monads: maybe, list, functor vs applicative vs monad, state

-}

data Maybe a = Just a | Nothing

head :: [a] -> a
head (x:xs) = x
head []     = error "ouch"

mhead :: [a] -> Maybe a
mhead (x:xs) = Just x
mhead []     = Nothing

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f Nothing  = Nothing
mmap f (Just x) = Just (f x)

madd1 :: Maybe Int -> Maybe Int
madd1 Nothing = Nothing
madd1 (Just i) = Just (1+i)
-- look familiar?

madd1' :: Maybe Int -> Maybe Int
madd1' = mmap (+1)

divide12by :: Int -> Int
divide12by i = 12 `div` i

mdivide12by :: Int -> Maybe Int
mdivide12by 0 = Nothing
mdivide12by i = Just (12 `div` i)

mdiv :: Maybe Int -> Maybe Int
mdiv Nothing = Nothing
mdiv (Just i) = mdivide12by i
-- look familiar? is it a map?

-- (=<<) , or (>>=) with args flipped
mbind :: (a -> Maybe b) -> Maybe a -> Maybe b
mbind f Nothing = Nothing
mbind f (Just x) = f x

mdiv' :: Maybe Int -> Maybe Int
mdiv' mx = mbind mdivide12by mx 

lmap :: (a -> b) -> [a] -> [b]
lmap f []     = []
lmap f (x:xs) = f x : lmap f xs

lbind :: (a -> [b]) -> [a] -> [b]
lbind f [] = []
lbind f (x:xs) = f x ++ lbind f xs

-- examples
-- lmap  (\x -> [x,x]) [1..5]
-- lbind (\x -> [x,x]) [1..5]

ljoin :: [[a]] -> [a]
ljoin [] = []
ljoin (xs:xss) = xs ++ ljoin xss
-- look familiar? is it a bind?

ljoin' :: [[a]] -> [a]
ljoin' xss = lbind id xss

-- we can also define bind using join and map, and map using bind and
-- pure/return
-- try it for lists or maybe

{- functors vs applicatives vs monads

functor     - map   :: (a -> b)   -> m a -> m b
applicative - (<*>) :: m (a -> b) -> m a -> m b
monad       - bind  :: (a -> m b) -> m a -> m b
-}

-- applicatives vs monads - rewind to lecture 11

-- the state monad

-- this material about state is straight from Hutton Section 12.3
-- (page 168), have a look there to find out more and see it in action

type State = Int

newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
  fmap g (S sf) = S (\olds -> let (x,news) = sf olds in (g x,news))

instance Applicative ST where
  pure x = S (\olds -> (x,olds))
  stf <*> stx = S (\ olds ->
      let (f,news)    = app stf olds
          (x,newests) = app stx news in (f x , newests))

instance Monad ST where
  st >>= f = S (\olds ->
     let (x,news) = app st olds
     in  app (f x) news)                
