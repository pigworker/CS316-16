module Total where

data List x
  =  Nil
  |  Cons x (List x)
  deriving Show

total :: List Int -> Int
total Nil          =  0                   -- [n]
total (Cons x xs)  =  x + total xs        -- [c]

ex0 :: Int
ex0 = total (Cons 1 (Cons 2 (Cons 3 Nil)))


