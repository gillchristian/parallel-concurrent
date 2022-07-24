module KMeans01 where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad.Par as Par
import Control.Monad.ST
import Control.Parallel.Strategies as Strategies
import Data.Array
import Data.Array.ST
import Data.Array.Unsafe as Unsafe
import Data.Binary (decodeFile)
import Data.Function
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Debug.Trace
import KMeansCore
import System.Environment
import System.IO
import System.Mem
import Text.Printf

runKmeans01 = runInUnboundThread $ do
  points <- decodeFile "points.bin"
  clusters <- read `fmap` readFile "clusters"
  let nclusters = length clusters
  args <- getArgs
  npoints <- evaluate (length points)
  performGC
  t0 <- getCurrentTime
  final_clusters <- case args of
    ["seq"] -> kmeansSeq nclusters points clusters
    ["strat", n] -> kmeansStrat (read n) nclusters points clusters
    -- ["par",     n] -> kmeans_par      (read n) nclusters points clusters
    -- ["divpar",  n] -> kmeans_div_par  (read n) nclusters points clusters npoints
    -- ["diveval", n] -> kmeans_div_eval (read n) nclusters points clusters npoints
    _other -> error "args"
  t1 <- getCurrentTime
  print final_clusters
  printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

tooMany = 80

kmeansSeq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansSeq nclusters points clusters =
  let loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        putStrLn "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = step nclusters clusters points
        if clusters' == clusters
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

kmeansStrat :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansStrat numChunks nclusters points clusters =
  let chunks = split numChunks points
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        -- printf "giving up."
        return clusters
      loop n clusters = do
        -- printf "iteration %d\n" n
        -- putStr (unlines (map show clusters))
        let clusters' = parStepsStrat nclusters clusters chunks
        if clusters' == clusters
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

-- -----------------------------------

-- step 1
assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
  vec <- MVector.replicate nclusters (PointSum 0 0 0)
  let addpoint p = do
        let c = nearest p
            cid = clId c
        ps <- MVector.read vec cid
        MVector.write vec cid $! addToPointSum ps p

  mapM_ addpoint points
  return vec
  where
    nearest p =
      fst $
        minimumBy
          (compare `on` snd)
          [(c, sqDistance (clCent c) p) | c <- clusters]

-- step 2
makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster i ps
    | (i, ps@(PointSum count _ _)) <- zip [0 ..] (Vector.toList vec),
      count > 0
  ]

step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points =
  makeNewClusters (assign nclusters clusters points)

parStepsStrat :: Int -> [Cluster] -> [[Point]] -> [Cluster]
parStepsStrat nclusters clusters pointss =
  makeNewClusters $
    foldr1
      combine
      (map (assign nclusters clusters) pointss `using` parList rseq)

split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where
    (as, bs) = splitAt n xs

addPointSums :: PointSum -> PointSum -> PointSum
addPointSums (PointSum c1 x1 y1) (PointSum c2 x2 y2) =
  PointSum (c1 + c2) (x1 + x2) (y1 + y2)

combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = Vector.zipWith addPointSums
