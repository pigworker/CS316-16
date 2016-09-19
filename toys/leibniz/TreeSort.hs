module TreeSort where

data List x
  =  Nil
  |  Cons x (List x)
  deriving Show

data Tree x
  =  Leaf
  |  Node (Tree x) x (Tree x)
  deriving Show

append :: List x -> List x -> List x
append  Nil          ys  =  ys                                       -- [a]
append  (Cons x xs)  ys  =  Cons x (append xs ys)                    -- [b]

insert :: Int -> Tree Int -> Tree Int
insert  x  Leaf                         =  Node Leaf x Leaf          -- [c]
insert  x  (Node lt y rt)  | x <= y     =  Node (insert x lt) y rt   -- [d]
                           | otherwise  =  Node lt y (insert x rt)   -- [e]

foldList :: t -> (x -> t -> t) -> List x -> t
foldList  n  c  Nil          =  n                                    -- [f]
foldList  n  c  (Cons x xs)  =  c x (foldList n c xs)                -- [g]

makeTree :: List Int -> Tree Int
makeTree = foldList Leaf insert                                      -- [h]

glueIn :: List x -> x -> List x -> List x
glueIn  xs  y  zs  =  append xs (Cons y zs)                          -- [i]

flatten :: Tree x -> List x
flatten  Leaf            =  Nil                                      -- [j]
flatten  (Node lt x rt)  =  glueIn (flatten lt) x (flatten rt)       -- [k]

compose :: (b -> c) -> (a -> b) -> a -> c
compose  f  g  a  =  f (g a)                                         -- [l]

sort :: List Int -> List Int
sort  =  compose flatten makeTree                                    -- [m]

ex0 :: List Int
ex0 = append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil))

ex1 :: List Int
ex1 = append (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil))) (Cons 5 (Cons 6 Nil))

ex2 :: Tree Int
ex2 = insert 3 Leaf

ex3 :: Tree Int
ex3 = insert 2 (insert 3 Leaf)

ex4 :: Tree Int
ex4 = insert 4 (insert 3 Leaf)

ex5 :: Tree Int
ex5 = insert 6 (Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 8 Leaf))

ex55 :: Int
ex55 = foldList 0 (+) (Cons 1 (Cons 2 (Cons 3 Nil)))

ex6 :: List Int
ex6 = foldList (Cons 5 (Cons 6 Nil)) Cons (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))

ex7 :: List Int
ex7 = foldList Nil append (Cons (Cons 1 (Cons 2 Nil)) (Cons (Cons 3 (Cons 4 Nil)) (Cons (Cons 5 (Cons 6 Nil)) Nil)))

ex8 :: Tree Int
ex8 = makeTree Nil

ex9 :: Tree Int
ex9 = makeTree (Cons 4 Nil)

ex10 :: Tree Int
ex10 = makeTree (Cons 7 (Cons 1 (Cons 6 (Cons 2 (Cons 5 (Cons 3 (Cons 4 Nil)))))))

ex11 :: List Int
ex11 = glueIn (Cons 1 (Cons 2 (Cons 3 Nil))) 4 (Cons 5 (Cons 6 (Cons 7 Nil)))

ex12 :: List Int
ex12 = glueIn (glueIn (Cons 1 Nil) 2 (Cons 3 Nil)) 4 (glueIn (Cons 5 Nil) 6 (Cons 7 Nil))

ex13 :: List Int
ex13 = flatten Leaf

ex14 :: List Int
ex14 = flatten (Node Leaf 3 Leaf)

ex15 :: List Int
ex15 = flatten (Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf)))

ex16 :: List Int
ex16 = compose (Cons 1) (Cons 2) Nil

ex17 :: List Int
ex17 = compose (compose (Cons 1) (Cons 2)) (Cons 3) Nil

ex18 :: List Int
ex18 = compose (Cons 1) (compose (Cons 2) (Cons 3)) Nil

ex19 :: List Int
ex19 = compose compose compose (Cons 1) append (Cons 2 Nil) (Cons 3 Nil)

ex20 :: List Int
ex20 = foldList Nil (compose Cons (1 +)) (Cons 1 (Cons 2 (Cons 3 Nil)))

ex21 :: List Int
ex21 = sort Nil

ex22 :: List Int
ex22 = sort (Cons 4 Nil)

ex23 :: List Int
ex23 = sort (Cons 7 (Cons 1 (Cons 6 (Cons 2 (Cons 5 (Cons 3 (Cons 4 Nil)))))))

ex24 :: List Int
ex24 = sort (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))


