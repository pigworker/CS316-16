module Lec10 where

{-
   BUILDING PURE EVALUATORS 
-}

-- From previous lecture

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

myExpr :: Expr
myExpr = (Number 2 `Add` Number 5) `Add` (Number 34 `Add` Number 12)

iterExpr :: (Int -> t) -> (t -> t -> t) -> Expr -> t
iterExpr number add (Number i) = number i
iterExpr number add (Add d e) =
    add (iterExpr number add d) (iterExpr number add e)

-- New material

evaluate :: Expr -> Int
evaluate = undefined

evalToList :: Expr -> [Int]
evalToList =  undefined

iterRight :: b -> (a -> b -> b) -> [a] -> b
iterRight nil cons []     = nil
iterRight nil cons (x:xs) = cons x (iterRight nil cons xs)

normalise :: Expr -> Expr
normalise = undefined

