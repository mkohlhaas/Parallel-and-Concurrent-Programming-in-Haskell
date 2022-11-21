{-# LANGUAGE ScopedTypeVariables #-}

-- K-Means sample from "Parallel and Concurrent Programming in Haskell"
--
-- With three versions:
--   [ kmeans_seq   ]  a sequential version
--   [ kmeans_strat ]  a parallel version using Control.Parallel.Strategies
--   [ kmeans_par   ]  a parallel version using Control.Monad.Par
--
-- Usage (sequential):
--   $ ./kmeans seq
--
-- Usage (Strategies):
--   $ ./kmeans strat 600 +RTS -N4
--
-- Usage (Par monad):
--   $ ./kmeans par 600 +RTS -N4
--
-- Usage (divide-and-conquer / Par monad):
--   $ ./kmeans divpar 7 +RTS -N4
--
-- Usage (divide-and-conquer / Eval monad):
--   $ ./kmeans diveval 7 +RTS -N4

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

-- read input files, time calculation
main = runInUnboundThread $ do
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
    ["par", n] -> kmeansPar (read n) nclusters points clusters
    ["divpar", n] -> kmeansDivPar (read n) nclusters points clusters npoints
    ["diveval", n] -> kmeansDivEval (read n) nclusters points clusters npoints
    _other -> error "args"
  t1 <- getCurrentTime
  print final_clusters
  printf "Total time: %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

-- repeatedly step until convergence (sequential)
kmeansSeq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansSeq nclusters points clusters =
  let loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        -- <1>
        putStrLn "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = step nclusters clusters points -- <2>
        if clusters' == clusters -- <3>
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

tooMany = 80

-- repeatedly step until convergence (Strategies)
kmeansStrat :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansStrat numChunks nclusters points clusters =
  let chunks = split numChunks points -- <1>
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do
        printf "giving up."
        return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = parStepsStrat nclusters clusters chunks
        if clusters' == clusters
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where
    (as, bs) = splitAt n xs

-- repeatedly step until convergence (Par monad)
kmeansPar :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansPar mappers nclusters points clusters =
  let chunks = split mappers points

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        printf "iteration %d\n" n
        putStr (unlines (map show clusters))
        let clusters' = stepsPar nclusters clusters chunks
        if clusters' == clusters
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

-- Use divide-and-conquer, and the Par monad for parallellism.
kmeansDivPar :: Int -> Int -> [Point] -> [Cluster] -> Int -> IO [Cluster]
kmeansDivPar threshold nclusters points clusters npoints =
  let tree = mkPointTree threshold points npoints

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let divconq :: Tree [Point] -> Par (Vector PointSum)
            divconq (Leaf points) = return $ assign nclusters clusters points
            divconq (Node left right) = do
              i1 <- spawn $ divconq left
              i2 <- spawn $ divconq right
              c1 <- get i1
              c2 <- get i2
              return $! combine c1 c2

            clusters' = makeNewClusters $ runPar $ divconq tree

        if clusters' == clusters
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

mkPointTree :: Int -> [Point] -> Int -> Tree [Point]
mkPointTree threshold points npoints = go 0 points npoints
  where
    go depth points npoints
      | depth >= threshold = Leaf points
      | otherwise =
        Node
          (go (depth + 1) xs half)
          (go (depth + 1) ys half)
      where
        half = npoints `quot` 2
        (xs, ys) = splitAt half points

-- Use divide-and-conquer, and the Eval monad for parallellism.
kmeansDivEval :: Int -> Int -> [Point] -> [Cluster] -> Int -> IO [Cluster]
kmeansDivEval threshold nclusters points clusters npoints =
  let tree = mkPointTree threshold points npoints

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
        hPutStr stderr (unlines (map show clusters))
        let divconq :: Tree [Point] -> Vector PointSum
            divconq (Leaf points) = assign nclusters clusters points
            divconq (Node left right) = runEval $ do
              c1 <- rpar $ divconq left
              c2 <- rpar $ divconq right
              rdeepseq c1
              rdeepseq c2
              return $! combine c1 c2

            clusters' = makeNewClusters $ divconq tree

        if clusters' == clusters
          then return clusters
          else loop (n + 1) clusters'
   in loop 0 clusters

-- Perform one step of the K-Means algorithm
step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points =
  makeNewClusters (assign nclusters clusters points)

assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
  vec <- MVector.replicate nclusters (PointSum 0 0 0)
  let addpoint p = do
        let c = nearest p; cid = clId c
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

data PointSum = PointSum {-# UNPACK #-} !Int {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance NFData PointSum where
  rnf (PointSum count xs ys) = () -- all fields are strict

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y) =
  PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) =
  Cluster
    { clId = i,
      clCent = Point (xs / fromIntegral count) (ys / fromIntegral count)
    }

addPointSums :: PointSum -> PointSum -> PointSum
addPointSums (PointSum c1 x1 y1) (PointSum c2 x2 y2) =
  PointSum (c1 + c2) (x1 + x2) (y1 + y2)

combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = Vector.zipWith addPointSums

parStepsStrat :: Int -> [Cluster] -> [[Point]] -> [Cluster]
parStepsStrat nclusters clusters pointss =
  makeNewClusters $
    foldr1
      combine
      (map (assign nclusters clusters) pointss `using` parList rseq)

stepsPar :: Int -> [Cluster] -> [[Point]] -> [Cluster]
stepsPar nclusters clusters pointss =
  makeNewClusters $
    foldl1' combine (runPar $ Par.parMap (assign nclusters clusters) pointss)

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster i ps
    | (i, ps@(PointSum count _ _)) <- zip [0 ..] (Vector.toList vec),
      count > 0
  ]

-- >>
-- v. important: filter out any clusters that have
-- no points.  This can happen when a cluster is not
-- close to any points.  If we leave these in, then
-- the NaNs mess up all the future calculations.
