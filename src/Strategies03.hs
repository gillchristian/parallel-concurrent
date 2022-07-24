module Strategies03 where

import Control.DeepSeq (force)
import Control.Parallel.Strategies hiding
  ( Strategy,
    evalList,
    parList,
    parPair,
    rdeepseq,
    using,
  )

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

type Strategy a = a -> Eval a

parPair :: Strategy (a, b)
parPair (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

parPair' :: Strategy (a, b)
parPair' = evalPair rpar rpar

rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)

parPair'' :: Strategy a -> Strategy b -> Strategy (a, b)
parPair'' sa sb = evalPair (rparWith sa) (rparWith sb)

deepParPair :: (NFData a, NFData b) => Strategy (a, b)
deepParPair = parPair'' rdeepseq rdeepseq

using :: a -> Strategy a -> a
x `using` s = runEval (s x)

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq

evalList :: Strategy a -> Strategy [a]
evalList _ [] = return []
evalList strat (x : xs) = do
  x' <- strat x
  xs' <- evalList strat xs
  return (x' : xs')

parList :: Strategy a -> Strategy [a]
parList strat = evalList (rparWith strat)

runStrategies03 =
  print $ show ((fib 35, fib 36) `using` parPair)
