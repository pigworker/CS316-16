module Lec05 where

{-
    RECURSIVE FUNCTIONS
-}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) | x <= y    = x : y : ys
                  | otherwise = y : insert x ys

{-
   insert 2 [1,3]
 =
   1 : insert 2 [3]
-}


isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lessers ++ [x] ++ qsort greaters where
  lessers  = [y | y <- xs, y < x]
  greaters = [y | y <- xs, y >= x]

{-
   qsort [3,5,1,2,4]
 =
   qsort [1,2] ++ [3] ++ qsort [5,4]
 =
   (qsort [] ++ [1] ++ qsort [2]) ++ [3] ++ (qsort [4] ++ [5] ++ qsort [])
 =
   ([] ++ [1] ++ ([] ++ [2] ++ [])) ++ [3] ++ (([] ++ [4] ++ []) ++ [5] ++ [])
 =
                                       [3]
          [1]                                                       [5]
       []     [2]                                     [4]                  []
            []    []                             []        []
 =
   ..
 =
   [1,2,3,4,5]
-} 


data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving Show

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Leaf           = Node Leaf x Leaf
insertBST x (Node lt y rt) | x < y     = Node (insertBST x lt) y rt
                           | otherwise = Node lt y (insertBST x rt) 


insertAll :: Ord a => [a] -> BST a
insertAll [] = Leaf
insertAll (x : xs) = insertBST x (insertAll xs)

insertBwds :: Ord a => BST a -> [a] -> BST a
insertBwds acc [] = acc
insertBwds acc (x : xs) = insertBwds (insertBST x acc) xs

flatten :: BST a -> [a]
flatten Leaf = []
flatten (Node lt a rt) = flatten lt ++ [a] ++ flatten rt

append [] ys = ys
append (x : xs) ys = x : append xs ys

{- flatten (Node someEnournousTree 72364527645 Leaf)
  =
    flatten bigTree ++ [48756834]
-}

treesort :: Ord a => [a] -> [a]
treesort xs = flatten (insertAll xs)

