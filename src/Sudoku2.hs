module Sudoku2 where

import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies
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

      (as, bs) = splitAt (length puzzles `div` 2) puzzles -- <1>
      solutions = runEval $ do
        as' <- rpar (force (map solve as)) -- <2>
        bs' <- rpar (force (map solve bs)) -- <2>
        rseq as' -- <3>
        rseq bs' -- <3>
        return (as' ++ bs') -- <4>
  print (length (filter isJust solutions))

data Tree a = Nil | Branch (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf Nil = ()
  rnf (Branch l a r) = rnf l `seq` rnf a `seq` rnf r
