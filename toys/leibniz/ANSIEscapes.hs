module ANSIEscapes where

data Dir = UpDir | DownDir | RightDir | LeftDir

instance Show Dir where
  show UpDir    = "A"
  show DownDir  = "B"
  show RightDir = "C"
  show LeftDir  = "D"

upLine            = putStr "\ESC[1A"
downLine          = putStr "\ESC[1B"

up                = moveCursor UpDir
down              = moveCursor DownDir
backward          = moveCursor LeftDir
forward           = moveCursor RightDir

moveCursor        :: Dir -> Int -> IO ()
moveCursor dir 0  = return ()
moveCursor dir n  = putStr $ "\ESC[" ++ show n ++ show dir

killLine          = escape "K" 
restoreCursor     = escape "u"
saveCursor        = escape "s"
clearScreen       = escape "2J"
initTermSize      = (escape "[=3h")

resetCursor       = escape "0;0H"

escape e          = putStr $ "\ESC[" ++ e

type Colour = Char

fg :: Colour -> String
fg c = "\ESC[3" ++ c : "m"
bg :: Colour -> String
bg c = "\ESC[4" ++ c : "m"

black, red, green, yellow, blue, magenta, cyan, white, plain :: Colour
black    = '0'
red      = '1'
green    = '2'
yellow   = '3'
blue     = '4'
magenta  = '5'
cyan     = '6'
white    = '7'
plain    = ' '

fgOn :: Colour -> Colour
fgOn c | elem c [black, red, blue, green]  = white
       | otherwise   = black

bold = "\ESC[1m"
inverse = "\ESC[7m"
normal = "\ESC[0m"