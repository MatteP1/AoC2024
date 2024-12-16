module Main where

import Data.Map.Strict as Map ( Map, fromList, toList, (!) )
import Data.Maybe (fromJust)
import Data.List (find, sortOn )
import Data.List.Index
import Linear.V2
import Algorithm.Search

data Direction =
    North
  | West
  | East
  | South
    deriving (Eq, Show, Ord)

type Pos = V2 Int

changeX :: Pos -> Int -> Pos
changeX pos delta = pos + V2 delta 0

changeY :: Pos -> Int -> Pos
changeY pos delta = pos + V2 0 delta

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

getNextPos :: Pos -> Direction -> Pos
getNextPos fromPos direction = 
  case direction of
        North -> changeY fromPos (-1)
        East -> changeX fromPos (1)
        West -> changeX fromPos (-1)
        South -> changeY fromPos (1)

type Node = (Pos, Char)

type State = (Node, Direction)

newtype Board = Board (Map Pos Char)

showBoard :: Board -> String
showBoard (Board board) =
  let entries = toList board
      sorted = sortOn (\(V2 px py, _) -> (py, px)) entries
      buildStringNl :: [(Pos, Char)] -> String
      buildStringNl [] = ""
      buildStringNl xs@((V2 _ firstY, _):_) = 
        reverse $ fst $ foldl (\(str, prevY) (V2 _ cy, cc) -> (if cy > prevY then cc:'\n':str else cc:str, cy))
                              ("", firstY)
                              xs
    in
      buildStringNl sorted

instance Show Board where
  show board = showBoard board

parseBoardInput :: [[Char]] -> Board
parseBoardInput input =
  let rows_indexed = indexed input
      boardList = map (\(y, row) -> map (\(x, c) -> ((V2 x y), c)) (indexed row)) rows_indexed
      indexed_input = concat boardList
  in
    Board $ fromList indexed_input

neighbors :: Board -> State -> [State]
neighbors (Board board) = \((pos, _), _) -> 
  let above = changeY pos (-1)
      right = changeX pos (1)
      left = changeX pos (-1)
      down = changeY pos (1)
  in
  filter (\((_, c), _) -> c /= '#')
  [ ((above, board ! above), North),
    ((right, board ! right), East),
    ((left, board ! left), West),
    ((down, board ! down), South)
  ]

transitionCost :: State -> State -> Int
transitionCost ((n1Pos, _), d1) ((n2Pos, _), _) = 
  if getNextPos n1Pos d1 == n2Pos then 1 else 1001

found :: State -> Bool
found ((_, c), _) = c == 'E'

findInitialState :: Board -> Maybe State
findInitialState (Board board) = do
  (pos, _) <- find (\(_, c) -> c == 'S') $ toList board
  return ((pos, 'S'), East)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let board = parseBoardInput $ lines input
      startState = fromJust $ findInitialState board
      solution = dijkstra (neighbors board) transitionCost found startState
    in do
    print $ fst $ fromJust solution