{- CS203 Topics in Computer Science 2 -}
{- Topic 4: Structured Interaction -}

{- This topic is about applications programming in Haskell. So far,
we've been concentrating on expressing *algorithms* (sorting, parsing,
interpreting) in Haskell. It's high time we packaged these
computations up inside some sort of user-interface.

We'll develop tools for displaying and editing structured data, starting
from basic layout management, and hopefully working up to a fully interactive
editor environment.
-}

{- Episode 1: Displaying Sized Blocks of Data -}

module Block where

import ANSIEscapes


{- For the most part, applications display data to us in rectangular regions.
Often, these regions are further subdivided, horizontally or vertically
(header bar, menu bar, scroll bars, main display).

We can model these regions as two-dimensional blocks with a given size.
Blocks can be subdivided horizontally or vertically into smaller blocks,
until sooner or later, we find the actual "stuff" to be displayed.
-}

type Layout a = (Size, Block a) -- the block should fit the size

type Size = (Int, Int)  -- width, height

data Block a
  = Stuff a
  | Blank                      -- fits any size
  | Fg Colour (Block a)
  | Bg Colour (Block a)
  | Ver (Layout a) (Layout a)  -- should both have the right width
                               -- and heights which add correctly
  | Hor (Layout a) (Layout a)  -- should both have the right height
                               -- and widths which add correctly
  deriving Show




{- To deliver a rectangular textual display, we need to produce a list of
strings all the same length -- the lines of our output. -}

type Box = [String]     -- each String should be the same length

{- If we know how to turn sized "stuff" into boxes, we can turn Blocks
into boxes. -}

boxLayout :: (Colour, Colour) -> ((Size, a) -> Box) -> Layout a -> Box
boxLayout bf boxa (xy, Stuff a)    = boxa (xy, a)
boxLayout bf boxa (xy, Blank)      = boxChar ' ' xy
boxLayout (b, f) boxa (xy, Fg f' ib)
  | f' == plain  = boxLayout (b, f) boxa (xy, Fg (fgOn b) ib)
  | f' == f      = boxLayout (b, f) boxa (xy, ib)
  | otherwise    = map wrap (boxLayout (b, f') boxa (xy, ib)) where
    wrap s = fg f' ++ s ++ fg f
boxLayout (b, f) boxa (xy, Bg b' ib)
  | b' == b      = boxLayout (b, f) boxa (xy, ib)
  | otherwise    = map wrap (boxLayout (b', f) boxa (xy, ib)) where
    wrap s = bg b' ++ s ++ bg b
boxLayout bf boxa (xy, Ver lb rb)
  = boxLayout bf boxa lb ++ boxLayout bf boxa rb
boxLayout bf boxa (xy, Hor lb rb)
  = zipWith (++) (boxLayout bf boxa lb) (boxLayout bf boxa rb)

fgl :: Colour -> Layout a -> Layout a
fgl c (xy, b) = (xy, Fg c b)

bgl :: Colour -> Layout a -> Layout a
bgl c (xy, b) = (xy, Bg c b)

{- Here's how to make a Box which repeats a given character. This is
especially useful for making a Blank Box. -}

boxChar :: Char -> Size -> Box
boxChar c (x, y) = replicate y (replicate x c)

{- A simple choice of "stuff" is Box itself. -}

boxBox :: (Size, Box) -> Box
boxBox (_, b) = b  -- we hope the size is right

layout :: (Colour, Colour) -> Layout Box -> Box
layout (b, f) = map wrap . boxLayout (b, f) boxBox where
  wrap s = bg b ++ fg f ++ s ++ normal

{- use this to print a Box -}

printBox :: Box -> IO ()
printBox = mapM_ putStrLn

{- use this to print a Layout Box -}

printLayout :: Layout Box -> IO ()
printLayout = printBox . layout (white, black)

{- SMART CONSTRUCTORS for layouts: they guarantee correct sizes! -}

blank :: Size -> Layout a
blank xy = (xy, Blank)

layZ :: Layout a
layZ = blank (0, 0)

hGap :: Int -> Layout a
hGap x = blank (x, 0)

vGap :: Int -> Layout a
vGap y = blank (0, y)

layChar :: Char -> Size -> Layout Box
layChar c xy = (xy, Stuff (boxChar c xy))

layS :: String -> Layout Box  -- what are we assuming about the string?
layS s = ((length s, 1), Stuff [s])

joinH :: Layout a -> Layout a -> Layout a
joinH b1@((x1, y1), _) b2@((x2, y2), _)
  | y1 == y2 = ((x1 + x2, y1), Hor b1 b2)
  | y1 < y2
  = ((x1 + x2, y2), Hor ((x1, y2), Ver b1 ((x1, y2 - y1), Blank)) b2)
  | y1 > y2
  = ((x1 + x2, y1), Hor b1 ((x2, y1), Ver b2 ((x2, y1 - y2), Blank)))

joinV :: Layout a -> Layout a -> Layout a
joinV b1@((x1, y1), _) b2@((x2, y2), _)
  | x1 == x2 = ((x1, y1 + y2), Ver b1 b2)
  | x1 < x2
  = ((x2, y1 + y2), Ver ((x2, y1), Hor b1 ((x2 - x1, y1), Blank)) b2)
  | x1 > x2
  = ((x1, y1 + y2), Ver b1 ((x1, y2), Hor b2 ((x1 - x2, y2), Blank)))
