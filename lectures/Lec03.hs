module Lec03 where

-- 3 Defining Functions


-- declarations versus definitions
--   a declaration introduces new types or values
--   a definition gives a new way to compute existing values

-- declaration

data MyType = Foo | Bar MyType deriving Show

-- definition



-- definition by equation (Fred)
--   without parameters

myVal :: MyType
myVal = Bar (Bar Foo)

--   with parameters

baz :: MyType -> MyType
baz x = Bar (Bar x)

--   using conditional expression (e.g., not)

not0 :: Bool -> Bool
not0 x = if x then False else True

-- replacing conditionals on the right by guards on the left (Conor)
--   e.g., greatest common divisor (gcd is already a thing, so rename)

gcd' :: Int -> Int -> Int
gcd' x y | x == y = x
         | x < y  = gcd' x (y - x)
         | otherwise = gcd' (x - y) y

--   also, not via guards

not1 :: Bool -> Bool
not1 x | x = False
       | otherwise = True

-- pattern matching (Fred)
--   e.g., not not again? (underscore patterns)

not2 :: Bool -> Bool
not2 True = False
not2 _ = True

--   e.g., maybeApply, maybe?  (:info Maybe, then use shplit,
--     with underscore expressions)

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just g) (Just y) = Just (g y)
maybeApply _ _ = Nothing

-- pattern matching on lists (Conor)
--   e.g., append
--   (chant and be happy)
--   e.g., reverse (the slow way)
--   e.g., sawPrefixOff

append :: [x] -> [x] -> [x]
append [] ys = ys
append (x : xs) ys =    x : append xs ys
  --   [1,2,3] [4,5,6]  1   [2,3,4,5,6]
  --   x = 1
  --   xs = [2,3]

rev :: [x] -> [x]
rev [] = []
rev (x : xs) = append (rev xs) [x]
 -- [1,2,3]     rev xs = [3,2]
 --   x = 1
 --   xs = [2,3]

sawPrefixOff :: Eq a => [a] -> [a] -> Maybe [a]
sawPrefixOff [] ys = Just ys
sawPrefixOff (x : pre) (y : ys) | x == y = sawPrefixOff pre ys
sawPrefixOff _ _ = Nothing
