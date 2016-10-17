module Lec09 where

data Nat
  = Zero
  | Succ Nat
  deriving Show

iterNat :: a -> (a -> a) -> Nat -> a
iterNat = undefined

plus :: Nat -> Nat -> Nat
plus = undefined


plus2 :: Nat -> Nat -> Nat
plus2 m n = iterNat -- 'a = Nat'
               undefined -- zero case
               undefined -- succ case
               m

plus3 :: Nat -> Nat -> Nat
plus3 = undefined

plus4 :: Nat -> Nat -> Nat
plus4 = undefined

eqNat :: Nat -> Nat -> Bool
eqNat = undefined

caseNat :: a -> (Nat -> a) -> Nat -> a
caseNat = undefined

-- can we do both together?

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat = undefined

-- is it really more powerful?

recNatFromIterNat :: a -> (Nat -> a -> a) -> Nat -> a
recNatFromIterNat = undefined

----------------------------------------------------------------------
data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

myExpr :: Expr
myExpr = (Number 2 `Add` Number 5) `Add` (Number 34 `Add` Number 12)

iterExpr :: (Int -> val) -> (val -> val -> val) -> Expr -> val
iterExpr = undefined

evaluate :: Expr -> Int
evaluate = undefined

evalToList :: Expr -> [Int]
evalToList =  undefined

iterRight :: b -> (a -> b -> b) -> [a] -> b
iterRight nil cons []     = nil
iterRight nil cons (x:xs) = cons x (iterRight nil cons xs)

normalise :: Expr -> Expr
normalise = undefined
