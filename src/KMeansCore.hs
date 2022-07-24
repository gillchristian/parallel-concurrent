--
-- Adapted from the K-Means example in the remote-0.1.1 package,
--   (c) Jeff Epstein <jepst79@gmail.com>
--
{-# LANGUAGE DeriveDataTypeable #-}

module KMeansCore where

import Control.DeepSeq
import Data.Binary
import qualified Data.ByteString.Char8 as B
import Data.Data (Data)
import Data.Function
import Data.List
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Read, Eq)

instance NFData Point where
  rnf (Point _ _) = () -- all fields are strict

zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1 - x2) ^ 2) + ((y1 - y2) ^ 2)

instance Binary Point where
  put (Point a b) = put a >> put b
  get = do
    a <- get
    b <- get
    pure (Point a b)

readPoints :: FilePath -> IO [Point]
readPoints f = do
  s <- B.readFile f
  let ls = map B.words $ B.lines s
      points =
        [ Point (read (B.unpack sx)) (read (B.unpack sy))
          | (sx : sy : _) <- ls
        ]
  --
  pure points

data Cluster = Cluster
  { clId :: {-# UNPACK #-} !Int,
    clCent :: {-# UNPACK #-} !Point
  }
  deriving (Show, Read, Eq)

instance NFData Cluster where
  rnf (Cluster _ _) = () -- all fields are strict

makeCluster :: Int -> [Point] -> Cluster
makeCluster clid points =
  Cluster
    { clId = clid,
      clCent = Point (a / fromIntegral count) (b / fromIntegral count)
    }
  where
    pointsum@(Point a b) = foldl' addPoint zeroPoint points
    count = length points

    addPoint :: Point -> Point -> Point
    addPoint (Point a b) (Point c d) = Point (a + c) (b + d)

data PointSum
  = PointSum
      {-# UNPACK #-} !Int -- # of points
      {-# UNPACK #-} !Double -- xs sum
      {-# UNPACK #-} !Double -- ys sum

instance NFData PointSum where
  rnf (PointSum _ _ _) = () -- all fields are strict

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y) =
  PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) =
  Cluster
    { clId = i,
      clCent = Point (xs / fromIntegral count) (ys / fromIntegral count)
    }
