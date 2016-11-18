module Lec18 where

import Prelude hiding (const, take, iterate)

{-
   LECTURE 18 : Lazy evaluation and Infinite Data
-}

{- A simple function: -}

inc :: Int -> Int
inc n = n + 1

{- How is the following evaluated?

     inc (2*3)
-}

{- 'Call by Value'

       inc (2*3)
     =              { multiply 2 and 3 }
       inc 6
     =              { definition of 'inc' }
       6 + 1
     =              { add }
       7
-}

{- 'Call by Name'

       inc (2*3)
     =              { definition of 'inc' }
       (2*3) + 1
     =              { multiply }
       6 + 1
     =              { add }
       7
-}

{- Could, in theory, mix Call-by-Value and Call-by-Name, but most
   languages pick one or the other. -}

{--------------------------------------------------------------------}
{- Termination behaviour.

   Is there any difference between the two strategies. For programs
   that terminate under both, no, but there is a difference when we
   have programs that always terminate. -}

neverFinish :: Int
neverFinish = 1 + neverFinish

const :: a -> b -> a
const a b = a

{- What does this do?

      const 1 neverFinish
-}

{- 'Call by Value'

       const 1 neverFinish
     =
       const 1 (1 + neverFinish)
     =
       const 1 (1 + (1 + neverFinish))
     =
       ...
     =
       const 1 (1 + ... (1 + neverFinish))
     =
       ...
-}

{- 'Call by Name'

       const 1 neverFinish
     =                        { definition of 'const' }
       1
-}

{- If there is *any* evaluation sequence that terminates, then CBN will
   also terminate and give the same answer. -}

{--------------------------------------------------------------------}
{- Sharing

   Naive Call-by-Name would lead to repeated work: -}

square :: Int -> Int
square x = x * x

{-
    square (2*3)
  =                  { definition of square }
    (2*3) * (2*3)
  =                  { multiply }
    6 * (2*3)
  =                  { multiply }
    6 * 6
  =                  { multiply }
    36
-}

{- But with Call-by-Name:

     square (2*3)
  =                  { multiply }
     square 6
  =                  { definition of square }
     6 * 6
  =                  { multiply }
     36
-}

{- So Haskell uses sharing:

     square (2*3)
   =                          { give '2*3' a name }
     let x = 2*3 in square x
   =                          { definition of square }
     let x = 2*3 in x * x
   =                          { multiply (forced by '*') }
     let x = 6 in x * x
   =                          { fetch 'x' }
     let x = 6 in 6 * 6
   =                          { multiply }
     let x = 6 in 36
   =                          { garbage collect }
     36
-}

{- This evaluation strategy is called 'Lazy evaluation':

      - Expressions are not evaluated until needed.
      - Expressions are not evaluated more than once.
-}

{--------------------------------------------------------------------}
{- Laziness, Procrastination, and Strictness -}

sumList :: Int -> [Int] -> Int
sumList accum []     = accum
sumList accum (x:xs) = sumList (accum + x) xs

{- Evaluation under Call-by-Value:

     sumList 0 [1,2,3]
   =
     sumList (0+1) [2,3]
   =
     sumList 1 [2,3]
   =
     sumList (1+2) [3]
   =
     sumList 3 [3]
   =
     sumList (3+3) []
   =
     sumList 6 []
   =
     6
-}

{- Evaluation under Call-by-Name and Lazy Evaluation:

     sumList 0 [1,2,3]
   =
     sumList (0+1) [2,3]
   =
     sumList ((0+1)+2) [3]
   =
     sumList (((0+1)+2)+3) []
   =
     ((0+1)+2)+3
   =
     (1+2)+3
   =
     3+3
   =
     6
-}

{- For long lists, the 0+1+2+3+4+5+... builds up and is not evaluated
   until the end of the list. This can consume a large amount memory,
   and is known as a "space leak".

   This can lead to surprising behaviour sometimes, and can cause
   seemingly simple programs to run out of memory and crash.

   The fix in this case is to use strict application:
-}

sumStrict :: Int -> [Int] -> Int
sumStrict accum []     = accum
sumStrict accum (x:xs) = (sumStrict $! (accum+x)) xs

{- The strict application operator:

      ($!) :: (a -> b) -> a -> b

   is 'magic' in the sense that it cannot be implemented in 'normal'
   Haskell. It evaluates the second argument before applying the
   function to it. With strict evaluation we get the 'Call-by-Value'
   behaviour as above. -}

{--------------------------------------------------------------------}
{- Infinite Data

   A benefit of lazy evaluation is the ease of handling infinite data.

   Here is an infinite list: -}

upFrom :: Int -> [Int]
upFrom i = i : upFrom (i+1)

{- Trying to print this out never terminates, but we can use various
   other functions to slice off bits of it: -}

take :: Int -> [a] -> [a]
take 0 _      = []
take n (x:xs) = x : take (n-1) xs

{- This behaviour can be useful for making programs more modular. Here
   is an example of finding square roots by generating an infinite
   list of approximations and then, separately, deciding how to cut it
   off (taken from "Why Functional Programming Matters" by John
   Hughes:

      https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
-}

next :: Double -> Double -> Double
next n x = (x + n/x)/2

{- If we can find a non-zero value 'x' such that

       next n x = x

   Then we have the square root of 'n'. This is because:

        (x + n/x)/2 = x
    <=>
        x + n/x = 2*x
    <=>
        n/x = x
    <=>
        n = x*x

   From the theory of Newton-Raphson root finding algorithms, we can
   approximate this value by starting from some initial guess and then
   repeatedly apply 'next n'. -}

-- From the standard library
iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f (f a)

{- So 'iterate (next n) i' will give us an infinite list of
   approximations.

   But how do we know when to stop? -}

-- Cutting off when the difference is less than 'epsilon'
within :: Double -> [Double] -> Double
within eps (a:b:xs) | abs (a-b) < eps = b
within eps (_:b:xs)                   = within eps (b:xs)

findSqrt :: Double -> Double
findSqrt n = within 0.0000001 (iterate (next n) 1)



-- Cutting off when the ratio is close to 1, works better for numbers
-- that are very small
relative :: Double -> [Double] -> Double
relative eps (a:b:xs) | abs (a/b - 1) < eps = b
relative eps (_:b:xs)                       = relative eps (b:xs)

findSqrt2 :: Double -> Double
findSqrt2 n = relative 0.0000001 (iterate (next n) 1)
