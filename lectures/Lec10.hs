module Lec10 where

{- LECTURE 10: BUILDING PURE EVALUATORS 

   We ended the previous lecture looking at a data type of arithmetic
   expressions: -}

data Expr
  = Number Int
  | Add    Expr Expr
  deriving Show

{- 'Expr's are binary trees, with 'Int's at the leaves and every node
   labeled with 'Add'. Here is an example 'Expr':-}

myExpr :: Expr
myExpr = (Number 2 `Add` Number 5) `Add` (Number 34 `Add` Number 12)

{- representing the expression:

      (2 + 5) + (34 + 12)

   Note that the bracketing is important. Even though we know that it
   does not matter what order we bracket actual addition, we are not
   doing actual addition yet. 'Expr' is a type of _abstract_ syntax
   trees for expressions.

   In Lecture 9, we also derived the iterator function for computing
   functions recursively over 'Expr's: -}

iterExpr :: (Int -> t) -> (t -> t -> t) -> Expr -> t
iterExpr number add (Number i) = number i
iterExpr number add (Add d e) =
    add (iterExpr number add d) (iterExpr number add e)

iterList :: b -> (a -> b -> b) -> [a] -> b
iterList nil cons []     = nil
iterList nil cons (x:xs) = cons x (iterList nil cons xs)

----------------------------------------------------------------------

{- 1. Semantics via iteration. -}
evaluate :: Expr -> Int
evaluate = undefined

-- evalToList

-- normalise by evalToList















{- 2. Expressions with exceptions -}

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  deriving Show



















{- 3. Expressions with printing -}

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  deriving Show


