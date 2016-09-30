module Lec4 where

import Prelude hiding (concat,map,filter,lookup)

squares :: [Int]
squares = [ x ^ 2 | x <- [1..5] ]

allpairs :: [(Int,Int)]
allpairs = [ (x,y) | x <- [1,2,3], y <- [4,5] ]

ordpairs :: [(Int,Int)]
ordpairs = [ (x,y) | x <- [1..3], y <- [x..3] ]

{-
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss
-}

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs ]

{-
firsts :: [(a,b)] -> [a]
firsts ps = [ x | (x,_) <- ps ]
-}

firsts :: [(a,b)] -> [a]
firsts ps = [ fst p | p <- ps ]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

map :: (a -> b) -> [a] -> [b]
map f xs = [ f x | x <- xs ]

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [ x | x <- xs, p x ]

{-
-- parallel (requires an extension)
zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = [(x,y) | x <- xs | y <- ys ]
-}

lookup :: Eq a => a -> [(a,b)] -> [b]
lookup k t = [ v | (k',v) <- t, k == k' ]

-- string comprehensions

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

-- db example from 'craft of functional programming'

type Person = String
type Book = String

type Database = [ (Person,Book) ]

exampleDB :: Database
exampleDB = [("Alice", "Tintin"),("Anna","Little Women"),("Alice","Asterix"),("Rory","Tintin")]

books :: Database -> Person -> [Book]
books db per = [ book | (per',book) <- db, per == per' ]

borrowers :: Database -> Book -> [Person]
borrowers db book = [ per | (per,book') <- db, book == book' ]


-- joining two files, quick hack in ghci
{-
readFile "file1"
lines it
map (splitOn ":") it
let emails = it

readFile "file2"
lines it
map (splitOn ",") it
let tutorials = it


[ (firstname,email,date,place) | [_,_,_,studentid,_,firstname,email,_,_] <- emails, [studentid',_,_,date,place,_] <- tutorials, studentid == studentid' ]
-}
