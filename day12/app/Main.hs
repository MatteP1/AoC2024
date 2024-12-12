module Main where

import Data.Graph
import Data.Tree
import Data.Map.Strict ( Map, fromList, toAscList, mapKeys, foldrWithKey, lookup )
import Data.List.Index
import Data.Maybe
import Control.Monad (guard)
import Data.List (groupBy, sort)
import Debug.Trace

-- Problem is modelled as a DAG (just like day10).

type Pos = (Int, Int)

type PlotType = Char

type Plot = (PlotType, Pos)

type Region = (PlotType, [Pos])

data Direction = North | East | West | South 
  deriving (Eq, Show)

type FencePiece = (Direction, Pos)

newtype Garden = Garden (Map Pos PlotType)

showGarden :: Garden -> String
showGarden (Garden garden) =
  let gardenList = toAscList $ mapKeys (\(x,y) -> (y,x)) garden
      gardenSize = maximum $ map (\((x, _), _) -> x) gardenList
  in
    foldl (\acc ((_, y), c) -> acc ++ (show c) ++ if y == gardenSize then "\n" else "") "" gardenList

instance Show Garden where
  show garden = showGarden garden

parseInput :: [[Char]] -> Garden
parseInput input =
  let rows_indexed = indexed input
      gardenList = map (\(y, row) -> map (\(x, c) -> ((x, y), c)) (indexed row)) rows_indexed
      indexed_input = concat gardenList
  in
    Garden $ fromList indexed_input

changeX :: Pos -> Int -> Pos
changeX pos delta = (fst pos + delta, snd pos)

changeY :: Pos -> Int -> Pos
changeY pos delta = (fst pos, snd pos + delta)

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

-- Create adjacency list from input: [(node, key, [key])], where node is plotType of position, key is position, and [key] are the positions next to key, such that their node values are 1 greater than key's node value

adjacentPositions :: Pos -> [Pos]
adjacentPositions pos = [changeX pos 1, changeX pos (-1), changeY pos 1, changeY pos (-1)]

findAdjacentNodes :: Pos -> PlotType -> Garden -> [Pos]
findAdjacentNodes pos plotType (Garden garden) = 
  let posMaybe adjPos = do 
        adjPlotType <- Data.Map.Strict.lookup adjPos garden
        guard (adjPlotType == plotType)
        return adjPos
    in
    mapMaybe (\adjPos -> posMaybe adjPos) $ adjacentPositions pos

adjacencyListFromGarden :: Garden -> [(PlotType, Pos, [Pos])]
adjacencyListFromGarden (Garden garden) =
  foldrWithKey (\pos plotType adjList -> (plotType, pos, (findAdjacentNodes pos plotType (Garden garden))) : adjList) [] garden

computeAreaOfRegion :: Region -> Int
computeAreaOfRegion (_, positions) = length positions

computePerimeterOfRegion :: Region -> Int
computePerimeterOfRegion (_, plotPositions) = 
  sum $ map (\pos -> length $ filter (\p -> not $ p `elem` plotPositions) $ adjacentPositions pos) plotPositions

computeSidesOfRegion :: Region -> Int
computeSidesOfRegion (_, plotPositions) = 
  let fencePiecesFromPos pos = [(West, changeX pos 1),
                           (East, changeX pos (-1)),
                           (South, changeY pos 1),
                           (North, changeY pos (-1))]
      regionFencePiecesFromPos pos = filter (\(_, p) -> not $ p `elem` plotPositions) $ fencePiecesFromPos pos
      allRegionFencePieces = concatMap (regionFencePiecesFromPos) plotPositions
      
      -- TODO: fix groupBy
      groupFencePieces fencePieces = groupBy (\(f1d, (f1px, f1py)) (f2d, (f2px, f2py)) -> f1d == f2d && ((f1px == f2px && (f1d == East || f1d == West)) || (f1py == f2py && (f1d == North || f1d == South)))) fencePieces
      -- TODO: fix groupBy
      
      fencePieceGroups = traceShowId $ groupFencePieces allRegionFencePieces -- TODO: removed trace

      fencePieceGroupsOrderable = map (\fpg -> map (\(d, (x, y)) -> case d of {North -> x; South -> x; West -> y; East -> y}) fpg) fencePieceGroups
      
      countGroupsOfConsecutiveElements list =
        let inner [] = 0
            inner (x:ls) = snd $ foldl (\(prev, count) y -> (y, if prev + 1 == y then count else count + 1)) (x, 1) ls
        in inner $ sort list
  in
    sum $ map countGroupsOfConsecutiveElements fencePieceGroupsOrderable

main :: IO ()
main = do
  input <- readFile "test1.txt"
  let garden = parseInput $ lines input
      gardenAdjList = adjacencyListFromGarden garden
      (gardenGraph, nodeFromVertex) = graphFromEdges' gardenAdjList

      -- Part 1. Do SCC decomposition to find the regions. Then compute Area and Perimeter of each region.
      regions = map (\l -> (case nodeFromVertex $ head l of (plotType, _, _) -> plotType, map (\v -> case (nodeFromVertex v) of (_, pos, _) -> pos) l)) $ map flatten $ scc gardenGraph :: [Region]

      fenceCosts = map (\r -> computeAreaOfRegion r * computePerimeterOfRegion r) regions

      -- Part 2
      discountedFenceCosts = map (\r -> computeAreaOfRegion r * computeSidesOfRegion r) regions


    in do
      print $ sum fenceCosts
      print $ sum discountedFenceCosts