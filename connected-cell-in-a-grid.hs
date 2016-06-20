-- https://www.hackerrank.com/challenges/connected-cell-in-a-grid
module Main where

import Control.Monad (forM)
import Control.Monad (forM_)
import Control.Monad.State (State)
import Data.Array (Array)
import Data.Function (on)
import Data.Set (Set)
import System.IO (IO)

import qualified Control.Monad       as Monad
import qualified Control.Monad.State as State
import qualified Data.Array          as Array
import qualified Data.List           as List
import qualified Data.Set            as Set

newtype Matrix
    = Matrix
      { getCells :: Array (Integer, Integer) Bool -- ^ A True Bool means the cell is set
      }
    deriving (Eq, Show)

isSet :: Matrix -> (Integer, Integer) -> Bool
isSet = (Array.!) . getCells

widthAndHeight :: Matrix -> (Integer, Integer)        
widthAndHeight = plusOne . snd . Array.bounds . getCells
  where
    plusOne (a, b) = (a + 1, b + 1)

data Cluster
    = Cluster
      { ctrMembers :: [(Integer, Integer)]
      , ctrSize :: Integer
      }
    deriving (Eq, Show)

emptyCluster :: Cluster
emptyCluster = Cluster [] 0

withMember :: Cluster -> (Integer, Integer) -> Cluster
withMember (Cluster members size) rowCol = Cluster (rowCol : members) (size + 1)

newtype TestCase
    = TestCase
      { getMatrix :: Matrix
      }
    deriving (Eq, Show)

data ClusterState
    = ClusterState
      { csVisited :: Set (Integer, Integer)
      , csCurrent :: Cluster
      , csClusters :: [Cluster]
      }
    deriving (Eq, Show)

emptyClusterState :: ClusterState
emptyClusterState = ClusterState Set.empty emptyCluster []

addVisited :: (Integer, Integer) -> ClusterState -> ClusterState
addVisited rowCol (ClusterState visited current clusters) =
    ClusterState (Set.insert rowCol visited) (current `withMember` rowCol) clusters

unvisited :: ClusterState -> (Integer, Integer) -> Bool
unvisited (ClusterState visited _ _) rowCol = not (Set.member rowCol visited)

addNewCluster :: ClusterState -> ClusterState
addNewCluster c @ (ClusterState visited current clusters)
    -- Don't create empty clusters
    | current == emptyCluster = c
    | otherwise = ClusterState visited emptyCluster (current : clusters)

clusters :: ClusterState -> [Cluster]
clusters state =
    if state == emptyClusterState
    then []
    else (csCurrent state : csClusters state)

-- Includes diagonals & north/east/south/west. Assumes matrix is 0-indexed.
neighbors :: (Integer, Integer) -> (Integer, Integer) -> [(Integer, Integer)]
neighbors (height, width) (centerRow, centerCol) =
    [ (row, col)
    | dX <- [-1..1]
    , dY <- [-1..1]
    , (dX, dY) /= (0, 0) 
    , let row = centerRow + dX
    , let col = centerCol + dY
    , inBounds row col ]
  where
    inBounds row col = 0 <= row && 0 <= col && row < height && col < width

-- | DFS of graph, making sure no cell is visited more than once
cluster :: Matrix -> (Integer, Integer) -> State ClusterState ()
cluster matrix current = do
    state <- State.get
    
    -- Only work with unvisited nodes
    Monad.when ((isSet matrix current) && (unvisited state current)) (do
      -- Visit unvisited neighbors in sequence
      let neighs = neighbors (widthAndHeight matrix) current
      let unvisitedNeighbors = filter (unvisited state) neighs 

      State.modify (addVisited current)

      -- mapM -> DFS
      Monad.mapM_ (cluster matrix) unvisitedNeighbors)
    
-- | DFS the graph and choose the biggest cluster.
-- We don't have to track all the clusters, but this does not affect the worst
-- case space requirements because the cluster could be the entire graph.
solution :: TestCase -> Integer
solution (TestCase matrix) = ctrSize biggestCluster
  where
    points = Array.indices (getCells matrix)

    dfs = \startPoint -> do
        State.modify addNewCluster
        cluster matrix startPoint
    stateAggregation = Monad.mapM_ dfs points
    finalClusters = clusters (State.execState stateAggregation emptyClusterState)

    biggestCluster = List.maximumBy (compare `on` ctrSize) finalClusters

main :: IO ()
main = do
    let readInteger s = read s :: Integer
    numRows <- fmap readInteger getLine
    numColumns <- fmap readInteger getLine

    rows <- Monad.forM [0..numRows - 1] (\row -> do
        bits <- fmap words getLine
        pure [((row, column), v == "1") | (column, v) <- zip [0..] bits])

    let grid = Array.array ((0, 0), (numRows - 1, numColumns - 1)) (Monad.join rows)
    let test = TestCase (Matrix grid)
    print (solution test)
