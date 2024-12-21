module Main where

import Data.Map.Strict as Map ( Map, fromList, toList, (!), (!?) )
import Data.Maybe (fromJust, mapMaybe)
import Data.List (find, sortOn )
import Data.List.Index
import Linear.V2
import Algorithm.Search
import Control.Monad (guard)

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

type State = Node

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

parseBoardInput :: String -> Board
parseBoardInput input =
  let rows_indexed = indexed $ lines input
      boardList = map (\(y, row) -> map (\(x, c) -> ((V2 x y), c)) (indexed row)) rows_indexed
      indexed_input = concat boardList
  in
    Board $ fromList indexed_input

neighbors :: Board -> State -> [State]
neighbors (Board board) = \(pos, _) ->
  let above = changeY pos (-1)
      right = changeX pos (1)
      left = changeX pos (-1)
      down = changeY pos (1)
  in
  filter (\(_, c) -> c /= '#')
  [ (above, board ! above),
    (right, board ! right),
    (left, board ! left),
    (down, board ! down)
  ]

foundEnd :: State -> Bool
foundEnd (_, c) = c == 'E'

foundStart :: State -> Bool
foundStart (_, c) = c == 'S'

findEnd :: Board -> Maybe State
findEnd (Board board) = do
  (pos, _) <- find (\(_, c) -> c == 'E') $ toList board
  return (pos, 'E')

findStart :: Board -> Maybe State
findStart (Board board) = do
  (pos, _) <- find (\(_, c) -> c == 'S') $ toList board
  return (pos, 'S')

findDistanceFromE :: [(State, Int)] -> State -> Maybe Int
findDistanceFromE distances = 
  let ds = Map.fromList distances in
    \s -> ds !? s

manhattan :: Num a => V2 a -> V2 a -> a
manhattan (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

findCheatsD :: Board -> State -> Int -> [State]
findCheatsD (Board b) (pos, _) d =
  let statesWithinX dist = filter (\(p, c) -> c /= '#' && manhattan p pos <= dist) $ toList b -- kinda slow to consider all positions on the board. Could instead generate list of positions within manhattan distance 'dist' of 'pos'.
    in
      statesWithinX d

main :: IO ()
main = do
  input <- readFile "input.txt"
  let board = parseBoardInput input

      endState = fromJust $ findEnd board

      pathEtoS = (endState:) $ fromJust $ dfs (neighbors board) foundStart endState

      pathDistanceFromE = map (\(a,b) -> (b,a)) $ indexed $ pathEtoS

      distanceFromE = findDistanceFromE pathDistanceFromE

      bigCheats maxCheatDist = concat $ map (\(state@(pos,_), dist) ->
        mapMaybe 
          (\s@(p,_) -> do dfe <- distanceFromE s
                          let cheatSize = (dist - dfe) - manhattan pos p
                          guard (cheatSize >= 100)
                          return (state, s, cheatSize))
          (findCheatsD board state maxCheatDist))
        pathDistanceFromE
    in do
    print $ length (bigCheats 2) -- part 1
    print $ length (bigCheats 20) -- part 2