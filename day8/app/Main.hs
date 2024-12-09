module Main where

import Data.Map.Strict ( Map, fromList, toAscList, toList, mapKeys )
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Data.List.Index
import Control.Monad (liftM2)

type AntennaType = Char

type Pos = (Int, Int)

type Antenna = (Pos, AntennaType)

type AntennaGroup = ([Pos], AntennaType)

newtype Board = Board (Map Pos Char)

showBoard :: Board -> String
showBoard (Board board) =
  let boardList = toAscList $ mapKeys (\(x,y) -> (y,x)) board
      boardSize = maximum $ map (\((x, _), _) -> x) boardList
  in
    foldl (\acc ((_, y), c) -> acc ++ [c] ++ if y == boardSize then "\n" else "") "" boardList

instance Show Board where
  show board = showBoard board

parseInput :: [[Char]] -> Board
parseInput input =
  let rows_indexed = indexed input
      boardList = map (\(y, row) -> map (\(x, c) -> ((x, y), c)) (indexed row)) rows_indexed
      indexed_input = concat boardList
  in
    Board $ fromList indexed_input

changeX :: Pos -> Int -> Pos
changeX pos delta = (fst pos + delta, snd pos)

changeY :: Pos -> Int -> Pos
changeY pos delta = (fst pos, snd pos + delta)

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

computeAntinodesPos :: Pos -> Pos -> Maybe (Pos, Pos)
computeAntinodesPos a1pos a2pos =
  if a1pos == a2pos then Nothing else
  let diffX = fst a1pos - fst a2pos 
      diffY = snd a1pos - snd a2pos
  in
    Just (changeXY a1pos diffX diffY, changeXY a2pos (-diffX) (-diffY))

computeAntinodes :: Antenna -> Antenna -> Maybe (Pos, Pos)
computeAntinodes a1@(a1pos, a1type) a2@(a2pos, a2type) =
  if a1 == a2 then Nothing else
  if a1type /= a2type then Nothing else
  let diffX = fst a1pos - fst a2pos 
      diffY = snd a1pos - snd a2pos
  in
    Just (changeXY a1pos diffX diffY, changeXY a2pos (-diffX) (-diffY))

findAntennas :: Board -> [Antenna]
findAntennas (Board board) = filter (\(_, c) -> c /= '.') $ toList board

findAntennaTypes :: [Antenna] -> [AntennaType]
findAntennaTypes antennas = nub $ map (\(_, atype) -> atype) antennas

groupAntennasByType :: [Antenna] -> [AntennaGroup]
groupAntennasByType antennas = 
  let atypes = findAntennaTypes antennas in
      map (\atype -> (map (\(p, _) -> p) $ filter (\(_, at) -> at == atype) antennas, atype)) atypes

cartProd :: [a] -> [b] -> [(a, b)]
cartProd = liftM2 (,)

findAntinodes :: AntennaGroup -> [Pos]
findAntinodes (agroup, _) =
  let pairs = cartProd agroup agroup
      antinodePairs = mapMaybe (\(p1, p2) -> computeAntinodesPos p1 p2) pairs
  in
    concat $ map (\(anp1, anp2) -> [anp1, anp2]) antinodePairs

isPosOutOfBounds :: Pos -> Board -> Bool
isPosOutOfBounds pos (Board board) = 
    let boardList = toList board
        maxX = maximum $ map (\((x, _), _) -> x) boardList
        maxY = maximum $ map (\((_, y), _) -> y) boardList
  in
  not $ fst pos <= maxX && snd pos <= maxY && 0 <= fst pos && 0 <= snd pos

filterOutOfBoundsPos :: [Pos] -> Board -> [Pos]
filterOutOfBoundsPos positions board =
  filter (\pos -> not $ isPosOutOfBounds pos board) positions



main :: IO ()
main = do
  input <- readFile "input.txt"
  let board = parseInput $ lines input
      antennas = findAntennas board
      antennaGroups = groupAntennasByType antennas
      antiNodes = concat $ map (\ag -> findAntinodes ag) antennaGroups
      antiNodesInBounds = filterOutOfBoundsPos antiNodes board
    in
    print $ length $ nub antiNodesInBounds
