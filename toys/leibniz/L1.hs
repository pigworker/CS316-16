module L1 where

data List x
  = Nil
  | Cons x (List x)
  deriving Show

append :: List x -> List x -> List x
append Nil ys = ys                              -- [n]
append (Cons x xs) ys = Cons x (append xs ys)   -- [c]

ex0 :: List Int
ex0 = append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 5 Nil))
