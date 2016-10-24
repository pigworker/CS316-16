module Lec11 where

-- we want to define our own Applicative and Monad
import Prelude (Functor(..),Show(..), Either(..), Int, Bool(..), Maybe(..), (+), (++),undefined)



















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

apM :: Maybe (a -> b) -> Maybe a -> Maybe b
apM Nothing mx = Nothing
apM (Just f) Nothing = Nothing
apM (Just f) (Just x) = Just (f x)

evalExpr :: Expr -> Maybe Int
evalExpr (Number n) = Just n
evalExpr (Add e1 e2) = 
-- Just (+) :: Maybe (Int -> (Int -> Int))
  (Just (+)) `apM` (evalExpr e1) `apM` (evalExpr e2) -- :: Maybe (Int)
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

apL :: [a -> b] -> [a] -> [b]
apL fs as = [f a | f <- fs, a <- as]

evalExpr2 :: Expr2 -> [Int]
evalExpr2 (Number2 n)    = [n]
evalExpr2 (Add2 e1 e2)   = [(+)] `apL` evalExpr2 e1 `apL` evalExpr2 e2
evalExpr2 (Choice e1 e2) = evalExpr2 e1 ++ evalExpr2 e2
evalExpr2 Failure        = []

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>)  :: f (a -> b) -> f a -> f b

infixl 3 <*>

instance Applicative Maybe where
  pure = Just
  Nothing <*> mx = Nothing
  Just f  <*> mx = fmap f mx

instance Applicative [] where
  pure x = [x]
  fs <*> as = [f a | f <- fs, a <- as]

evalExpr' :: Expr -> Maybe Int
evalExpr' (Number n) = pure n
evalExpr' (Add e1 e2) = 
  (pure (+)) <*> evalExpr' e1 <*> evalExpr' e2
evalExpr' Throw = Nothing
evalExpr' (Catch e1 e2) =
  case evalExpr' e1 of
    Nothing -> evalExpr' e2
    Just n -> Just n

evalExpr2' :: Expr2 -> [Int]
evalExpr2' (Number2 n)    = pure n
evalExpr2' (Add2 e1 e2)   = pure (+) <*> evalExpr2' e1 <*> evalExpr2' e2
evalExpr2' (Choice e1 e2) = evalExpr2' e1 ++ evalExpr2' e2
evalExpr2' Failure        = []



-- Conor

data Expr3
  = Number3 Int
  | Add3    Expr3 Expr3
  | Boolean Bool
  | IfThenElse Expr3 Expr3 Expr3
  | Throw3
  deriving Show

type Value = Either Int Bool

intify :: Maybe Value -> Maybe Int
intify (Just (Left i)) = Just i
intify _ = Nothing

boolify :: Maybe Value -> Maybe Bool
boolify (Just (Right b)) = Just b
boolify _ = Nothing

cond :: Bool -> a -> a -> a
cond True t e = t
cond False t e = e

evalExpr3 :: Expr3 -> Maybe Value
evalExpr3 (Number3 i) = pure (Left i)
evalExpr3 (Add3 d e) = pure Left <*> (pure (+) <*> intify (evalExpr3 d) <*> intify (evalExpr3 e))
evalExpr3 (Boolean b) = pure (Right b)
evalExpr3 (IfThenElse e e0 e1) =
  boolify (evalExpr3 e) >>= \ b ->
  if b then evalExpr3 e0 else evalExpr3 e1
evalExpr3 Throw3 = Nothing

myE :: Expr3
myE = IfThenElse (Boolean True) (Add3 (Number3 2) (Number3 3)) Throw3

class Applicative f => Monad f where
  return :: a -> f a
  (>>=) :: f a -> (a -> f b) -> f b

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  Just a >>= f = f a
