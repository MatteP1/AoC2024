module Main where

import Data.Map.Strict ( (!?), mapAccumWithKey, Map, fromList )
import Data.Maybe (fromMaybe)
import Data.List.Index

type Pos = (Int, Int)

type Board = Map Pos Char

data Direction =
    North
  | West
  | East
  | South

type GuardState = (Pos, Direction)

type BoardState = (Board, GuardState)

-- TODO use state monad...

parseInput :: [[Char]] -> Board
parseInput input = 
  let rows_indexed = indexed input
      board = map (\(y, row) -> map (\(x, c) -> ((x, y), c)) (indexed row)) rows_indexed
      indexed_input = concat board
  in
    fromList indexed_input

changeX :: Pos -> Int -> Pos
changeX pos delta = (fst pos + delta, snd pos)

changeY :: Pos -> Int -> Pos
changeY pos delta = (fst pos, snd pos + delta)

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

directions :: [(Int, Int)]
directions = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]


main :: IO ()
main = putStrLn "Hello, Haskell!"
