module Lec07 where

import Prelude hiding
  (Maybe (..), Monoid, Foldable, Bool (..), String)

-- Declaring Types and Classes (Hutton, Ch. 8)

--------------------------------------------------------------
-- 1. Type synonyms

-- New names for old types

type String = [Char]

type Position = (Int, Int)

origin :: Position
origin = (0,0)

type Transformation = Position -> Position

type Position3d = (Float, Float, Float)


-- Not allowed:
-- type Tree2 = (Int, [Tree2])

-- Parameterisation

type Pair a = (a,a)
--        ^    ^ ^
--        |    `-`- uses
--        ` name

-- by analogy with:
pair :: a -> Pair a
pair a = (a,a)

-- type names start with captial letters

-- We use a parameterised type by applying it to a type
type Position2 = Pair Int

doesNothing :: (Int,Int) -> (Int,Int)
doesNothing x = x

-- Multiple parameters
type Tuple a b = (a,b)


-------------------------------------------------------------
-- 2. Data types
data Bool = True | False

-- again, type names start with capital letters
-- and so do constructor names ('True', 'False')


data Direction = North | South | East | West
  deriving Show

move :: Direction -> Transformation
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

moves :: [Direction] -> Transformation
moves []         = id
moves (dir:dirs) = moves dirs . move dir


data Maybe a = Nothing | Just a

safeDivision :: Int -> Int -> Maybe Int
safeDivision x 0 = Nothing
safeDivision x y = Just (x `div` y)

-- Recursive datatypes

data Nat = Zero | Succ Nat

zero = Zero
one  = Succ Zero
two  = Succ (Succ Zero)
three = Succ two


data List a = Nil | Cons a (List a)




data Tree a = Leaf | Node (Tree a) a (Tree a)




data LeafyTree a b = Leafy b
                   | Nodey (LeafyTree a b) a (LeafyTree a b)




data RoseTree a = Rose a [RoseTree a]

rose = Rose 1 []


data XML = Element String [XML]
         | Text String

type MaybeRoseTree a = Maybe (RoseTree a)

emptyRoseTree :: MaybeRoseTree Int
emptyRoseTree = Nothing





data NEList a = NEList a (Maybe (NEList a))

onetwothree :: NEList Int
onetwothree = NEList 1 (Just (NEList 2 (Just (NEList 3 Nothing))))

--emp :: NEList Int
--emp = ???

head :: [a] -> Maybe a
head []     = Nothing
head (x:xs) = Just x

headNE :: NEList a -> a
headNE (NEList a _) = a

-------------------------------------------------------------
-- Type classes

class Monoid b where
  base :: b
  op :: b -> b -> b
  -- op base x == x
  -- op x base == x
  -- op (op x y) z == op x (op y z)

instance Monoid [x] where
  base = []
  op = (++)

instance Monoid Int where
  base = 0
  op = (+)


-- foldMaybe
foldList :: Monoid b => (a -> b) -> [a] -> b
foldList measure [] = base
foldList measure (a : as) =
  op (measure a) (foldList measure as)
-- foldTree
foldTree :: Monoid b => (a -> b) -> Tree a -> b
-- monoid
foldTree measure Leaf = base
foldTree measure (Node l a r) =
  op (foldTree measure l)
     (op (measure a) (foldTree measure r))

myTree :: Tree Int
myTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- Newtypes

newtype Multy = MkMulty {unMulty :: Int}

instance Monoid Multy where
  base = MkMulty 1
  op (MkMulty x) (MkMulty y) = MkMulty (x * y)


-- Any, All, DList

-- Foldables?
