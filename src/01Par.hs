module 01Par where

import Control.Exception
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock
import System.Environment
import Text.Printf

-- Parallel and Concurrent Programming in Haskell (https://simonmar.github.io/pages/pcph.html)
--
-- LICENSE: https://github.com/simonmar/parconc-examples/blob/master/LICENSE
--
-- Chapter 2 Exercises

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

run01Par = do
  [n] <- getArgs
  let test = [test1, test2, test3, test4] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0

test1 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  return (x, y)

test2 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  return (x, y)

test3 = do
  x <- rpar (fib 36)
  y <- rseq (fib 35)
  rseq x
  return (x, y)

test4 = do
  x <- rpar (fib 36)
  y <- rpar (fib 35)
  rseq x
  rseq y
  return (x, y)

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
