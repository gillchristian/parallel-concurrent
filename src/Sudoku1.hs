module Sudoku1 where

import Control.Exception
import Data.Maybe
import Sudoku
import System.Environment

-- Parallel and Concurrent Programming in Haskell (https://simonmar.github.io/pages/pcph.html)
--
-- LICENSE: https://github.com/simonmar/parconc-examples/blob/master/LICENSE
--
-- Chapter 2 - Sudoku Example

runSudoku :: IO ()
runSudoku = do
  [f] <- getArgs
  file <- readFile f
  let puzzles = lines file
      solutions = map solve puzzles
  print (length (filter isJust solutions))
