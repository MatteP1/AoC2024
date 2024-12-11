module Main where

import Data.Graph
import Data.Tree
import Data.Map.Strict ( Map, fromList, toAscList, mapKeys, foldrWithKey, lookup )
import Data.List.Index
import Data.Maybe
import Control.Monad (guard)

-- Problem is modelled as a DAG.

type Pos = (Int, Int)

type Height = Int

newtype TopographicalMap = TopoMap (Map Pos Height)

showTopoMap :: TopographicalMap -> String
showTopoMap (TopoMap topoMap) =
  let topoMapList = toAscList $ mapKeys (\(x,y) -> (y,x)) topoMap
      topoMapSize = maximum $ map (\((x, _), _) -> x) topoMapList
  in
    foldl (\acc ((_, y), c) -> acc ++ (show c) ++ if y == topoMapSize then "\n" else "") "" topoMapList

instance Show TopographicalMap where
  show topoMap = showTopoMap topoMap

parseInput :: [[Char]] -> TopographicalMap
parseInput input =
  let rows_indexed = indexed input
      topoMapList = map (\(y, row) -> map (\(x, c) -> ((x, y), read . pure $ c)) (indexed row)) rows_indexed
      indexed_input = concat topoMapList
  in
    TopoMap $ fromList indexed_input

changeX :: Pos -> Int -> Pos
changeX pos delta = (fst pos + delta, snd pos)

changeY :: Pos -> Int -> Pos
changeY pos delta = (fst pos, snd pos + delta)

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

-- Create adjacency list from input: [(node, key, [key])], where node is height of position, key is position, and [key] are the positions next to key, such that their node values are 1 greater than key's node value

findAdjacentNodes :: Pos -> Height -> TopographicalMap -> [Pos]
findAdjacentNodes pos height (TopoMap topoMap) = 
  let adjecentPositions = [changeX pos 1, changeX pos (-1), changeY pos 1, changeY pos (-1)]
      posMaybe adjPos = do 
        adjHeight <- Data.Map.Strict.lookup adjPos topoMap
        guard (adjHeight == height + 1)
        return adjPos
    in
    mapMaybe (\adjPos -> posMaybe adjPos) adjecentPositions

adjacencyListFromTopographicalMap :: TopographicalMap -> [(Height, Pos, [Pos])]
adjacencyListFromTopographicalMap (TopoMap topoMap) =
  foldrWithKey (\pos height adjList -> (height, pos, (findAdjacentNodes pos height (TopoMap topoMap))) : adjList) [] topoMap

pathTreeFromVertex :: Vertex -> Graph -> Tree Vertex -- part 2
pathTreeFromVertex vertex g = 
  let find1Reach v = map (snd) $ filter (\(v', _) -> v' == v)(edges g) in
    unfoldTree (\v -> (v, find1Reach v)) vertex

-- From adjacency list, create graph (graphFromEdges). For each trailhead, find each reachable 9 height positions. Count and add.

main :: IO ()
main = do
  input <- readFile "input.txt"
  let topoMap = parseInput $ lines input
      topoMapAdjList = adjacencyListFromTopographicalMap topoMap
      (topoGraph, nodeFromVertex) = graphFromEdges' topoMapAdjList
      trailheadVertices = filter (\vertex -> case (nodeFromVertex vertex) of (h, _, _) -> h == 0) $ vertices topoGraph

      -- Part 1 is basically reachability in the DAG. For each trailhead node, find the reachable nodes, and filter for the ones with height 9. The length of the resulting list is the trailheads score.

      calculateScore vertex = length $ filter (\(h, _, _) -> h == 9) $ map (nodeFromVertex) (reachable topoGraph vertex)

      trailheadScores = map calculateScore trailheadVertices

      -- Part 2 is solved by computing "path trees" for each trailhead node. A path tree is a tree of all possible paths one can take starting from some node. The rating of a trailhead node is then the number of leafs in its path tree with a node of height 9.

      pathTrees = map (\v -> pathTreeFromVertex v topoGraph) trailheadVertices
      
      calculateRatingFromPathTree pathTree = length $ filter (\v -> case (nodeFromVertex v) of (h, _, _) -> h == 9) (flatten pathTree)

      trailheadRatings = map calculateRatingFromPathTree pathTrees

    in do
      print $ sum trailheadScores
      print $ sum trailheadRatings
