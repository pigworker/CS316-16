-- lecture 21 -- concurrency, threads, mvars

import Control.Concurrent
import System.IO
import Text.Printf
import Control.Monad

-- forkIO :: IO () -> IO ThreadId
{-
main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 100000 (putChar ' '))
  replicateM_ 100000 (putChar 'B')

-- threadDelay :: Int -> IO ()

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)
  printf "%d seconds is up! BING!\a\a\a" t

main = loop
  where
    loop = do
      s <- getLine
      if s == "exit"
         then return ()
         else do forkIO $ setReminder s
                 loop


-- MVars

{- interface

newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()
-}

main = do
  m <- newEmptyMVar
  forkIO $ putMVar m 'x'
  r <- takeMVar m
  print r

main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'
              putMVar m 'y' -- removing this causes exception
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
-}

-- logger example

{-
data Logger

initLogger :: IO Logger
logMessage :: Logger -> String -> IO ()
logStop    :: Logger -> IO ()

-}

data Logger = Logger (MVar LogCommand)

data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
  where
    loop = do
      threadDelay (10^6 * 1)
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          putStrLn msg
          loop
        Stop s -> do
          putStrLn "logger: stop"          
          putMVar s ()

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  putStrLn "We didn't wait for the log message"
  logMessage l "bye"
  logStop l
  putStrLn "We waited until the the log had stopped"
