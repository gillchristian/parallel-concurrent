module Sudoku3 where

import Control.Exception
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe
import Sudoku
import System.Environment

-- Parallel and Concurrent Programming in Haskell (https://simonmar.github.io/pages/pcph.html)
--
-- LICENSE: https://github.com/simonmar/parconc-examples/blob/master/LICENSE
--
-- Chapter 2 - Sudoku Example

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = pure []
parMap f (x : xs) = do
  a <- rpar (f x)
  as <- parMap f xs
  pure (a : as)

runSudoku :: IO ()
runSudoku = do
  [f] <- getArgs
  file <- readFile f
  let puzzles = lines file
  evaluate (length puzzles)
  let solutions = runEval $ parMap solve puzzles
  print (length (filter isJust solutions))
