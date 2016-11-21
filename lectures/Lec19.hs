-- lecture 19 -- testing

import Test.QuickCheck
import Data.List
import Control.Monad

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs = filter (< x) xs
        rhs = filter (>= x) xs

prop_idempotent :: (Num a,Ord a) => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs
        
prop_model :: (Num a,Ord a) => [a] -> Bool
prop_model xs = qsort xs == sort xs

data Color = R | B deriving Show

data RBTree a = Leaf
              | Node Color (RBTree a) a (RBTree a)
              deriving Show

-- leaves are black
-- can't have two reds in a row
-- black heights of subtrees should be the same

unbound :: Arbitrary a => Gen (RBTree a)
unbound = oneof [return Leaf,
                 Node <$> (oneof [return R,return B]) <*> unbound <*> arbitrary <*> unbound]

bounded :: Arbitrary a => Gen (RBTree a)
bounded = sized tree where
  tree :: Arbitrary a => Int -> Gen (RBTree a)
  tree 0 = return Leaf
  tree n | n > 0 = oneof [return Leaf,
    Node <$> (oneof [return R,return B]) <*> tree (n `div` 2) <*> arbitrary <*> tree (n `div` 2)]

prop_NoRedRed :: Ord a => RBTree a -> Bool
prop_NoRedRed Leaf = True
prop_NoRedRed (Node R (Node R _ _ _) _ _) = False
prop_NoRedRed (Node R _ _ (Node R _ _ _)) = False
prop_NoRedRed (Node _ l x r) = prop_NoRedRed l && prop_NoRedRed r

nrrTree :: Arbitrary a => Gen (RBTree a)
nrrTree = sized (tree R) where
  tree :: Arbitrary a => Color {- parent color -} -> Int -> Gen (RBTree a)
  tree B 0 = return Leaf
  tree B n | n > 0 = oneof [return Leaf,
    Node B <$> tree B (n `div` 2) <*> arbitrary <*> tree B (n `div` 2),
    Node R <$> tree R (n `div` 2) <*> arbitrary <*> tree R (n `div` 2)]                  
  tree R 0 = return Leaf
  tree R n | n > 0 = oneof [return Leaf,
    Node B <$> tree B (n `div` 2) <*> arbitrary <*> tree B (n `div` 2)]

-- quickCheck $ forAll nrrTree prop_NoRedRed
                   
