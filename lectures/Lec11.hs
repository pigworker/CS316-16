module Lec11 where

-- we want to define our own Applicative and Monad
import Prelude (Functor(..),Show(..), Either, Int, Bool, Maybe(..), (+), (++),undefined)



















{- LECTURE 11: Applicatives and Monads -}

-- Fred

data Expr
  = Number Int
  | Add    Expr Expr
  | Throw
  | Catch  Expr Expr
  deriving Show

{- An example program using Throw and Catch is this one: -}

myProblemProgram :: Expr
myProblemProgram =
  (Add (Number 12) Throw) `Catch` (Number 123)













evalExpr :: Expr -> Maybe Int
evalExpr (Number n) = Just n
evalExpr (Add e1 e2) =
  case evalExpr e1 of
    Nothing -> Nothing
    Just n1 ->
      case evalExpr e2 of
        Nothing -> Nothing
        Just n2 -> Just (n1 + n2)
evalExpr Throw = Nothing
evalExpr (Catch e1 e2) =
  case evalExpr e1 of
    Nothing -> evalExpr e2
    Just n -> Just n























-- Conor 

data Expr2
  = Number2 Int
  | Add2    Expr2 Expr2
  | Choice  Expr2 Expr2
  | Failure    
  deriving Show

myDitheringProgram :: Expr2
myDitheringProgram =
  (Choice (Number2 0) (Number2 1)) `Add2` (Number2 2)


evalExpr2 :: Expr2 -> [Int]
evalExpr2 (Number2 n)    = undefined
evalExpr2 (Add2 e1 e2)   = undefined
evalExpr2 (Choice e1 e2) = undefined
evalExpr2 Failure        = undefined








-- Fred (look in literature)




















-- Conor

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Boolean Bool
  | IfThenElse Expr3 Expr3 Expr3
  | Throw3
  deriving Show

type Value = Either Int Bool
