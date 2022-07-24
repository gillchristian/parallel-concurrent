module Sudoku5 where

import Control.Parallel.Strategies
import Data.Maybe
import Sudoku
import System.Environment

runSudoku :: IO ()
runSudoku = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
  let solutions = map solve puzzles `using` parList rseq

  print (length (filter isJust solutions))
