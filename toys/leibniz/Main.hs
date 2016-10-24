{-# LANGUAGE ForeignFunctionInterface #-}

-- use flag -lncurses to compile

module Main where

import Foreign
import Foreign.C (CInt(..))
import ANSIEscapes
import System.IO
import System.Exit
import System.Environment
import System.IO.Error

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Char(toLower, chr, ord)
import Block
import Overlay
import Tm
import HaLay

data Window = Window
type WindowPtr = Ptr Window


foreign import ccall
  initscr :: IO () 

foreign import ccall "endwin"
  endwin :: IO CInt

foreign import ccall "refresh"
  refresh :: IO CInt

foreign import ccall "&LINES"
  linesPtr :: Ptr CInt

foreign import ccall "&COLS"
  colsPtr :: Ptr CInt

scrSize :: IO (Int, Int)
scrSize = do
    lnes <- peek linesPtr
    cols <- peek colsPtr
    return (fromIntegral cols, fromIntegral lnes)

copies :: Int -> a -> [a]
copies n a = take n (repeat a)

crlf :: IO ()
crlf = putStr "\r\n"

putLn :: String -> IO ()
putLn x = putStr x >> crlf

type ScreenState = (Point, Size)
  -- position in buffer of top left corner of screen, screen size

onScreen :: Point -> ScreenState -> ScreenState
onScreen (cx, cy) ((px, py), s@(sw, sh))
  = (( intoRange px cx sw, intoRange py cy sh), s)
  where
    intoRange i j x
      | i <= j && j <= i + x = i   -- in range, no change
      | otherwise = max 0 (j - div x 2)

getEscapeKey :: [(String, Key)] -> IO (Maybe Key)
getEscapeKey [] = return Nothing
getEscapeKey sks = case lookup "" sks of
  Just k -> return (Just k)
  _ -> do
    c <- getChar
    getEscapeKey [(cs, k) | (d : cs, k) <- sks, d == c]

keyReady :: IO (Maybe Key)
keyReady = do
  b <- hReady stdin
  if not b then return Nothing else do
    c <- getChar
    case c of
      '\n' -> return $ Just Return
      '\r' -> return $ Just Return
      '\b' -> return $ Just Backspace
      '\DEL' -> return $ Just Backspace
      _ | c >= ' ' -> return $ Just (CharKey c)
      '\ESC' -> do
        b <- hReady stdin
        if not b then return $ Just Quit else do
          c <- getChar
          case c of
            '[' -> getEscapeKey escapeKeys
            _ -> return $ Just Quit
      _ -> return $ Nothing

whatAndWhere ::  ScreenState -> State -> (Layout Box, Point)
whatAndWhere (_, (w, _)) (_, tz, _, _) = (l, fromMaybe (0, 0) (cur l)) where
  l = choose (rendZ tz)
  choose [] = hGap 0
  choose [l] = l
  choose (l@((x, _), _) : ls) = if x <= w then l else choose ls
  cur (_, Bg c _) | c == black = Just (0, 0)
  cur (xy, Fg _ b) = cur (xy, b)
  cur (xy, Bg _ b) = cur (xy, b)
  cur (xy, Hor l@((d, _), _) r) = cur l <|> (\ (x, y) -> (x + d, y)) <$> cur r
  cur (xy, Ver t@((_, d), _) b) = cur t <|> (\ (x, y) -> (x, y + d)) <$> cur b
  cur _ = Nothing

data ArrowDir = UpArrow | DownArrow | LeftArrow | RightArrow
data Modifier = Normal | Shift | Control

data Key
  = CharKey Char                -- an ordinary printable character
  | ArrowKey Modifier ArrowDir  -- an arrow key
  | Return
  | Backspace
  | Delete
  | Quit

{- Keys come in as standard ANSI escape sequences. You can look 'em up
online. Feel free to extend escapeKeys so that more keystrokes get
translated. -}

directions :: [(Char, ArrowDir)]
directions = [('A', UpArrow), ('B', DownArrow),
              ('C', RightArrow), ('D', LeftArrow)]

escapeKeys :: [(String, Key)]
escapeKeys =
  [([c], ArrowKey Normal d) | (c, d) <- directions] ++
  [("1;2" ++ [c], ArrowKey Shift d) | (c, d) <- directions] ++
  [("1;5" ++ [c], ArrowKey Control d) | (c, d) <- directions] ++
  [("3~", Delete)]

{-Last but not least, you get to tell my code how much damage you've done.
This makes the redrawing more efficient: if you've done less damage to
the file, my code needs to do less to update. If in doubt, overestimate
the damage: a slow display is better than a broken display. -}

data Damage
  = NoChange       -- use this if nothing at all happened
  | PointChanged   -- use this if you moved the cursor but kept the text
  | LineChanged    -- use this if you changed text only on the current line
  | LotsChanged    -- use this if you changed text off the current line
  deriving (Show, Eq, Ord)

type State = ([(Char, Rule)], (TmC, Tm), Int, [Tm])

handleKey :: Key -> State -> Maybe (Damage, State)
handleKey (ArrowKey Normal UpArrow) (p, (InF c a, f), i, es)
  = Just (LotsChanged, (p, (c, f :$ a), i, es))
handleKey (ArrowKey Normal UpArrow) (p, (InA f c, a), i, es)
  = Just (LotsChanged, (p, (c, f :$ a), i, es))
handleKey (ArrowKey Normal LeftArrow) (p, (c, f :$ a), i, es)
  = Just (LotsChanged, (p, (InF c a, f), i, es))
handleKey (ArrowKey Normal RightArrow) (p, (c, f :$ a), i, es)
  = Just (LotsChanged, (p, (InA f c, a), i, es))
handleKey (ArrowKey Control DownArrow) (p, (c, f :$ a), i, e : es)
  = Just (LotsChanged, (p, (Root, e), i, es))
handleKey (CharKey '+') (p, (c, (F "+" :$ C x :$ C y)), i, es)
  = Just (LotsChanged, (p, (c, C (show ((read x + read y) :: Int))), i, es))
handleKey (CharKey k) (p, ct, i, es) = do
  r <- lookup (toLower k) p
  ct' <- rule r ct
  Just (LotsChanged, (p, ct', i, es))
handleKey Return (p, (Root, t), i, e : es)
  | done t = Just (LotsChanged, (p, (Root, e), i + 1, es))
  where
    done (C _) = True
    done (f :$ a) = done f && done a
    done _ = False
handleKey _ _ = Nothing

outer :: ScreenState -> State -> IO (Int, Int)
outer ps st = inner ps st (whatAndWhere ps st) LotsChanged
  where
  inner ps@(p, s) st lc@(l, c@(cx, cy)) d = do
    refresh
    s' <- scrSize
    let ps'@((px, py), (sw, _)) = onScreen c (p, s')
    let d' = if ps /= ps' then LotsChanged else d
    case d' of
      LotsChanged -> do
        clearScreen
        resetCursor
        mapM_ putStr (layout (white, black) (cropLay cropBox ps' l))
      LineChanged -> do
        resetCursor
        down (cy - py)
        mapM_ putStr (layout (white, black) (cropLay cropBox ((px, cy), (sw, 1)) l))
      _ -> return ()
    if d' > NoChange then case ps of
      (_, (x, y)) -> do
        resetCursor
        forward (x - 1)
        down (y - 1)
     else return ()
    mc <- keyReady
    case mc of
      Nothing -> inner ps' st lc NoChange
      Just Quit -> case st of (_, _, k, es) -> return (k, length es)
      Just k -> case handleKey k st of
        Nothing -> inner ps' st lc NoChange
        Just (d, st') -> inner ps' st' (whatAndWhere ps' st') d

myGC :: Handle -> IO String
myGC h = do
  m <- catchIOError (hGetChar h >>= \ c -> return (Just c)) $ \ _ -> return Nothing
  case m of
    Nothing -> return ""
    Just c -> (c :) <$> myGC h

num :: String -> IO Int
num f = catchIOError
  (do h <- openFile f ReadMode
      s <- myGC h
      hClose h
      if length s > 0 then return (read s) else return 0)
  $ \ _ -> return 0

vigenere :: Char -> Char -> Char
vigenere k c = chr (97 + mod (ord k + ord c - 97) 26)

erenegiv :: Char -> Char -> Char
erenegiv k c = chr (97 + mod (ord c - 97 - ord k) 26)

main = do 
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  u <- catchIOError (getEnv "USER") $ \ _ -> return "outis"
  xs <- getArgs
  x <- case xs of
    [x] -> return x
    _ -> putStrLn "I need a file to work with!" >> exitFailure
  let v = zipWith vigenere (cycle x) u
  s <- catchIOError (readFile x) $ \ _ ->
         putStrLn ("Error reading " ++ x) >> exitFailure
  k <- num (".thrylce"++x++".count")
  initscr
  let tss = ready "" s
  s' <- scrSize
  let (prog, exs) = (mkProgram tss, mkExamples tss ++ [C "That's all, folks!" :$ (C "Password" :$ F v)])
  (k', j) <- case drop k exs of e : es -> outer ((0, 0), s') (prog, (Root, e), k, es)
  endwin
  writeFile (".thrylce"++x++".count") (show k')
  putStrLn $ "That's all, folks!" ++ if j < 1 then " (Password " ++ v ++ ")" else ""

--foreign import ccall unsafe "nomacro_getyx" 
--        nomacro_getyx :: Ptr Window -> Ptr CInt -> Ptr CInt -> IO ()

--standardScreen :: Window
--standardScreen = unsafePerformIO (peek stdscr)

--foreign import ccall "static &stdscr" 
--    stdscr :: Ptr Window


--getYX :: Ptr Window -> IO (Int, Int)
-- getYX w =
--     alloca $ \py ->                 -- allocate two ints on the stack
--         alloca $ \px -> do
--             nomacro_getyx w py px   -- writes current cursor coords
--             y <- peek py
--             x <- peek px
--             return (fromIntegral y, fromIntegral x)


