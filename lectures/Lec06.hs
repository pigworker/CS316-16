module Lec06 where
-- Lecture 6 - Higher order functions

import Prelude hiding (map, filter, reverse, (++), (.))

add :: Int -> Int -> Int
add x y = x + y

-- add 3 4 == 3 + 4 == 7

add' :: Int -> (Int -> Int)
add' x = \ y -> x + y

-- add' 3 4 == (\ y -> 3 + y) 4 == 3 + 4 == 7

add10 :: Int -> Int
add10 = add 10

-- add10 3

-- section style
add10' :: Int -> Int
add10' = (10+) -- (+10)

-- be careful with minus as -10 has another meaning
-- other ways to write add

add'' :: Int -> Int -> Int
add'' = (+)

add''' :: Int -> Int -> Int
add''' = \ x -> \ y -> x + y

-- syntactic sugar for add'''
add'''' :: Int -> Int -> Int
add'''' = \ x y -> x + y

--add'''' 3 4
--  == (\ x y -> x + y) 3 4
--  == ((\ x -> \ y -> x + y) 3) 4
--  == (\ y -> 3 + y) 4
-- == 3 + 4

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

twice' :: (Int -> Int) -> (Int -> Int)
twice' f = \ x -> f (f x)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- map (+1) [1..10]

map' :: (a -> b) -> [a] -> [b]
map' f xs = [ f x | x <- xs ]

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

-- filter even [1..10]

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- related to Cayley's theorem: Ints can be represented as functions from
-- Int -> Int e.g 3 can be represented as \x -> 3 + x, apply to 0 to get back

-- the same idea can be applied to lists
-- John Hughes (difference lists) (inventor of Quick Check)
-- a higher order representation of data
type DList a = [a] -> [a]

fromList :: [a] -> DList a
fromList xs = \l -> xs ++ l

toList :: DList a -> [a]
toList xs = xs []

nil :: DList a -- = [a] -> [a]
nil = \ xs -> xs

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

append :: DList a -> DList a -> DList a
xs `append` ys = xs . ys

reverse' :: [a] -> [a]
reverse' l = toList (rev l) where
  rev :: [a] -> DList a
  rev [] = nil
  rev (x:xs) = rev xs `append` fromList [x]

-- we can rewrite reverse' by expanding the definition of DList,
-- taking the extra argument (called acc for accumlator) that this
-- exposes and running any functions that we have enough information
-- to do so.

-- In the end this gives is exactly the fast accumulator
-- based version of reverse that is in the haskell prelude

reverse'' :: [a] -> [a]
reverse'' l = rev l [] where
  rev :: [a] -> [a] -> [a]
  rev []     acc = acc -- nil acc
  rev (x:xs) acc = rev xs (x:acc) -- (rev xs `append` fromList [x])


{- We were able to simplify the write hand sides of the rev function
   in reverse'' because:

           nil acc
       ==
          (\ xs -> xs) acc
       ==
          acc      
          (rev xs `append` fromList [x]) acc
       == 
          (rev xs . fromList [x]) acc
       ==
          rev xs (fromList [x] acc)
       ==
          rev xs ([x] ++ acc)
       ==
          rev xs (x:acc)
    -}
