{-# LANGUAGE RecordWildCards  #-}
--
-- INFOB3CC Concurrency
-- Practical 2: Single Source Shortest Path
--
--    Δ-stepping: A parallelisable shortest path algorithm
--    https://www.sciencedirect.com/science/article/pii/S0196677403000762
--
-- https://ics.uu.nl/docs/vakken/b3cc/assessment.html
--
-- https://cs.iupui.edu/~fgsong/LearnHPC/sssp/deltaStep.html
-- 

module DeltaStepping (

  Graph, Node, Distance,
  deltaStepping,

) where

import Sample
import Utils

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Bits
import Data.Graph.Inductive                                         ( Gr )
import Data.IORef
import Data.IntMap.Strict                                           ( IntMap )
import Data.IntSet                                                  ( IntSet )
import Data.Vector.Storable                                         ( Vector )
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Text.Printf
import qualified Data.Graph.Inductive                               as G
import qualified Data.IntMap.Strict                                 as Map
import qualified Data.IntSet                                        as Set
import qualified Data.Vector.Mutable                                as V
import qualified Data.Vector.Storable                               as M ( unsafeFreeze )
import qualified Data.Vector.Storable.Mutable                       as M


type Graph    = Gr String Distance  -- Graphs have nodes labelled with Strings and edges labelled with their distance
type Node     = Int                 -- Nodes (vertices) in the graph are integers in the range [0..]
type Distance = Float               -- Distances between nodes are (positive) floating point values


-- | Find the length of the shortest path from the given node to all other nodes
-- in the graph. If the destination is not reachable from the starting node the
-- distance is 'Infinity'.
--
-- Nodes must be numbered [0..]
--
-- Negative edge weights are not supported.
--
-- NOTE: The type of the 'deltaStepping' function should not change (since that
-- is what the test suite expects), but you are free to change the types of all
-- other functions and data structures in this module as you require.
--
deltaStepping
    :: Bool                             -- Whether to print intermediate states to the console, for debugging purposes
    -> Graph                            -- graph to analyse
    -> Distance                         -- delta (step width, bucket width)
    -> Node                             -- index of the starting node
    -> IO (Vector Distance)
deltaStepping verbose graph delta source = do
  threadCount <- getNumCapabilities             -- the number of (kernel) threads to use: the 'x' in '+RTS -Nx'

  -- Initialise the algorithm
  (buckets, distances)  <- initialise graph delta source
  printVerbose verbose "initialse" graph delta buckets distances

  let
    -- The algorithm loops while there are still non-empty buckets
    loop = do
      done <- allBucketsEmpty buckets
      if done
      then return ()
      else do
        printVerbose verbose "result" graph delta buckets distances
        step verbose threadCount graph delta buckets distances
        loop
  loop

  printVerbose verbose "result" graph delta buckets distances
  -- Once the tentative distances are finalised, convert into an immutable array
  -- to prevent further updates. It is safe to use this "unsafe" function here
  -- because the mutable vector will not be used any more, so referential
  -- transparency is preserved for the frozen immutable vector.
  --
  -- NOTE: The function 'Data.Vector.convert' can be used to translate between
  -- different (compatible) vector types (e.g. boxed to storable)
  --
  M.unsafeFreeze distances

-- Initialise algorithm state
--
initialise
    :: Graph
    -> Distance
    -> Node
    -> IO (Buckets, TentativeDistances)
initialise graph delta source = do
  -- All vertices initialise with infinite distance
  let amountOfNodes = G.size graph
  tentativeDistances <- M.replicate amountOfNodes infinity
  M.write tentativeDistances 0 0 --source has distance 0

-- All buckets start as empty sets, except B[0] which contains the source
  --determine size of array
  let (_,biggestDistance) = G.nodeRange graph --to test
      arraySize = 1+ fromIntegral biggestDistance / delta --to test
  bucketIndexRef <- newIORef 0
  bucket <- V.replicate (round arraySize) Set.empty
  let buckets = Buckets bucketIndexRef bucket
  V.modify (bucket_array buckets) (Set.insert source) --add source to b[0]
  return (buckets,tentativeDistances)

--All vertices hacve infinituve tentative ddistance except for s which is 0
--All buckets are empty except B[0] contains s

-- Take a single step of the algorithm.
-- That is, one iteration of the outer while loop.
--
step
    :: Bool
    -> Int
    -> Graph
    -> Distance
    -> Buckets
    -> TentativeDistances
    -> IO ()
step graph delta buck tent = do
-- Find index i of the first non-empty bucket.
  buckets@(Buckets bIndex bArray) <- readMVar buck
  firstIndex <- findNextBucket buckets
  oldTicket <- readForCAS bIndex
  incrementIndex bIndex oldTicket firstIndex
-- Repeatedly handle all outgoing light edges from nodes B[i]:
  handleEdges bArray firstIndex delta Set.empty
-- When the bucket remains empty, handle all outgoing heavy edges of nodes that have been in B[i]:
  -- • Find requests of light edges
  -- • Relax requests'
-- end step
  return ()
  where
    -- Repeatedly handle all outgoing light edges from nodes B[i]:
    handleEdges :: V.IOVector IntSet -> Int -> Distance -> IntSet -> IO()
    handleEdges bucketArray firstIndex d visitedNodes = do
      contentsAtIndex <- V.read bucketArray firstIndex
      if Set.null contentsAtIndex then return ()
      else do
        -- • Remove all nodes from B[i] (make it empty)
        V.write bucketArray firstIndex Set.empty
        (Buckets i _) <- takeMVar buck
        putMVar buck $ Buckets i bucketArray
        --let nodesOnIndex = Set.toAscList contentsAtIndex
        -- • Find requests of light edges
        lightRequests <- findRequests (<= d) graph contentsAtIndex tent
        -- • Relax requests'
        relaxRequests buck tent d lightRequests
        -- • Keep track of all nodes that have been in this bucket
        let newVisitedNodes = Set.union visitedNodes contentsAtIndex
        handleEdges bucketArray firstIndex d newVisitedNodes

    incrementIndex ioRef oldTicket firstIndex  = do
        (flag,_) <- casIORef ioRef oldTicket $ firstIndex + 1
        if flag then return () else incrementIndex ioRef oldTicket firstIndex


-- Once all buckets are empty, the tentative distances are finalised and the
-- algorithm terminates.
--
allBucketsEmpty :: Buckets -> IO Bool
allBucketsEmpty buck = do
    (Buckets first_index bucket_array) <- readMVar buck
    j <- readIORef first_index
    let numBuckets = V.length bucket_array -- Number of buckets
    buckets <- forM [0 .. numBuckets - 1] $ \i -> V.read bucket_array ((j + i) `rem` numBuckets)
    return (all (==True) (map checkBuckets buckets))

checkBuckets :: IntSet -> Bool
checkBuckets i = if i == Set.empty then True else False

-- Return the index of the smallest on-empty bucket. Assumes that there is at
-- least one non-empty bucket remaining.
--
findNextBucket :: Buckets -> IO Int
findNextBucket Buckets{..} = do
    firstIndex <- readIORef first_index --Read value IORef
    let numBuckets = V.length bucket_array -- Number of buckets
    buckets <- forM [0 .. numBuckets - 1] $ \i -> V.read bucket_array ((firstIndex + i) `mod` numBuckets) -- List of IntSets
    let nonEmptyBuckets = filter (/= Set.empty) buckets -- Filter the empty buckets
        cyclicBuckets = drop 1 $ cycle nonEmptyBuckets -- Creates cyclic list by dropping first element
        smallestIndex = maybe 0 (\i -> (firstIndex + i + 1) `mod` numBuckets) $ L.findIndex (/= Set.empty) cyclicBuckets -- Get index of first non-empty bucket, then get the position of this index in the vector
    return (smallestIndex)


-- Create requests of (node, distance) pairs that fulfil the given predicate
--
findRequests
    :: Int
    -> (Distance -> Bool)
    -> Graph
    -> IntSet
    -> TentativeDistances
    -> IO (IntMap Distance)
findRequests threadCount p graph v' distances = do
  tent <- readMVar distances --get the list of tentative distances from source
  let indexList = Set.toAscList v' 
  let neighborsOfN = map(G.neighbors graph) indexList
  
  distanceList <- mapM (\x ->M.read tent x) indexList --make a list of all distances from the selected nodes
  let distIndexList = zip indexList distanceList 
      filteredDistIndexList = filter (p.snd) distIndexList --filtered list with either the light or heavy edges

      filteredIntMap = Map.fromList filteredDistIndexList
  return filteredIntMap


-- Execute requests for each of the given (node, distance) pairs
--
relaxRequests
    :: Int
    -> Buckets
    -> TentativeDistances
    -> Distance
    -> IntMap Distance
    -> IO ()
relaxRequests buck tent delta req = do
  let nodeDistList = Map.toList req
  forM_ nodeDistList $ \(x, y) -> do  -- for each (node, distance) calculate relax
    relax buck tent delta (x, y)


-- Execute a single relaxation, moving the given node to the appropriate bucket
-- as necessary
--
relax :: Buckets
      -> TentativeDistances
      -> Distance
      -> (Node, Distance) -- (w, x) in the paper
      -> IO ()
relax buck safeTent delta (x, w) = do
  tent <- readMVar safeTent
  tentX <- M.read tent x
  (Buckets bIndex bArray) <- takeMVar buck
  currentBucket <- V.read bArray x
  if w < tentX           -- If distance smaller then tentDistance of node
    then do
      let updatedBucket = Set.delete x currentBucket -- Delete IntSet out of the current bucket
      V.write bArray x updatedBucket -- Update bucket_array
      putMVar buck $ Buckets bIndex bArray 
      let newBuckIndex = floor $ w / delta        -- calculate new bucket
      if newBuckIndex < V.length bArray           -- if it's in bounds
        then do
          newBucket <- V.read bArray newBuckIndex 
          let updatedNewBucket = Set.insert x newBucket -- Insert node in bucket
          (Buckets bIndex bArray) <- takeMVar buck
          V.write bArray newBuckIndex updatedNewBucket  -- Update bucket_array
          putMVar buck $ Buckets bIndex bArray
      else 
        return()
    else
      return ()



-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------
--
-- Here are a collection of (data)types and utility functions that you can use.
-- You are free to change these as necessary.
--

type TentativeDistances = M.IOVector Distance

data Buckets = Buckets
  { firstBucket   :: {-# UNPACK #-} !(IORef Int)           -- real index of the first bucket (j)
  , bucketArray   :: {-# UNPACK #-} !(V.IOVector IntSet)   -- cyclic array of buckets
  }


-- The initial tentative distance, or the distance to unreachable nodes
--
infinity :: Distance
infinity = 1/0


-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n action = do
  -- Fork the threads and create a list of the MVars which per thread tell
  -- whether the action has finished.
  finishVars <- mapM work [0 .. n - 1]
  -- Once all the worker threads have been launched, now wait for them all to
  -- finish by blocking on their signal MVars.
  mapM_ takeMVar finishVars
  where
    -- Create a new empty MVar that is shared between the main (spawning) thread
    -- and the worker (child) thread. The main thread returns immediately after
    -- spawning the worker thread. Once the child thread has finished executing
    -- the given action, it fills in the MVar to signal to the calling thread
    -- that it has completed.
    --
    work :: Int -> IO (MVar ())
    work index = do
      done <- newEmptyMVar
      _    <- forkOn index (action index >> putMVar done ())  -- pin the worker to a given CPU core
      return done


printVerbose :: Bool -> String -> Graph -> Distance -> Buckets -> TentativeDistances -> IO ()
printVerbose verbose title graph delta buckets distances = when verbose $ do
  putStrLn $ "# " ++ title
  printCurrentState graph distances
  printBuckets graph delta buckets distances
  putStrLn "Press enter to continue"
  _ <- getLine
  return ()

-- Print the current state of the algorithm (tentative distance to all nodes)
--
printCurrentState
    :: Graph
    -> TentativeDistances
    -> IO ()
printCurrentState graph distances = do
  printf "  Node  |  Label  |  Distance\n"
  printf "--------+---------+------------\n"
  forM_ (G.labNodes graph) $ \(v, l) -> do
    x <- M.read distances v
    if isInfinite x
       then printf "  %4d  |  %5v  |  -\n" v l
       else printf "  %4d  |  %5v  |  %f\n" v l x
  --
  printf "\n"

printBuckets
    :: Graph
    -> Distance
    -> Buckets
    -> TentativeDistances
    -> IO ()
printBuckets graph delta Buckets{..} distances = do
  first <- readIORef firstBucket
  mapM_
    (\idx -> do
      let idx' = first + idx
      printf "Bucket %d: [%f, %f)\n" idx' (fromIntegral idx' * delta) ((fromIntegral idx'+1) * delta)
      b <- V.read bucketArray (idx' rem V.length bucketArray)
      printBucket graph b distances
    )
    [ 0 .. V.length bucketArray - 1 ]

-- Print the current bucket
--
printCurrentBucket
    :: Graph
    -> Distance
    -> Buckets
    -> TentativeDistances
    -> IO ()
printCurrentBucket graph delta Buckets{..} distances = do
  j <- readIORef firstBucket
  b <- V.read bucketArray (j rem V.length bucketArray)
  printf "Bucket %d: [%f, %f)\n" j (fromIntegral j * delta) (fromIntegral (j+1) * delta)
  printBucket graph b distances

-- Print a given bucket
--
printBucket
    :: Graph
    -> IntSet
    -> TentativeDistances
    -> IO ()
printBucket graph bucket distances = do
  printf "  Node  |  Label  |  Distance\n"
  printf "--------+---------+-----------\n"
  forM_ (Set.toAscList bucket) $ \v -> do
    let ml = G.lab graph v
    x <- M.read distances v
    case ml of
      Nothing -> printf "  %4d  |   -   |  %f\n" v x
      Just l  -> printf "  %4d  |  %5v  |  %f\n" v l x
  --
  printf "\n"