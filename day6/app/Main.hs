module Main where

import Data.Map.Strict ( (!?), insert, Map, fromList, toAscList, toList, mapKeys )
import Data.Maybe (fromMaybe)
import Data.List (find, nub)
import Data.List.Index
import Control.Monad.Trans.State

type Pos = (Int, Int)

type Path = [Pos]

newtype Board = Board (Map Pos Char)

-- Quite hacky, but it works :)
showBoard :: Board -> String
showBoard (Board board) =
  let boardList = toAscList $ mapKeys (\(x,y) -> (y,x)) board
      boardSize = maximum $ map (\((x, _), _) -> x) boardList
  in
    foldl (\acc ((_, y), c) -> acc ++ [c] ++ if y == boardSize then "\n" else "") "" boardList

instance Show Board where
  show board = showBoard board

data Direction =
    North
  | West
  | East
  | South
    deriving (Eq, Show)

type Guard = (Pos, Direction)

type BoardState = (Board, Guard)

turnGuard :: Direction -> Direction
turnGuard dir = case dir of
  North -> East
  East -> South
  South -> West
  West -> North

isObstactle :: Board -> Pos -> Bool
isObstactle (Board board) pos = 
  let mb = do
        c <- board !? pos
        return (c == '#')
  in
    fromMaybe False mb

changeX :: Pos -> Int -> Pos
changeX pos delta = (fst pos + delta, snd pos)

changeY :: Pos -> Int -> Pos
changeY pos delta = (fst pos, snd pos + delta)

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

positionInFrontOfGuard :: Guard -> Pos
positionInFrontOfGuard (gpos, dir) = 
  case dir of
    North -> changeY gpos (-1)
    West -> changeX gpos (-1)
    East -> changeX gpos 1
    South -> changeY gpos 1

updateGuardDirection :: State BoardState Direction
updateGuardDirection = do
  (board, guard@(gpos, dir)) <- get
  let newgPos = positionInFrontOfGuard guard
  if isObstactle board newgPos then
    do
      put (board, (gpos, turnGuard dir))
      updateGuardDirection
  else
    return dir

insertGuardOnBoard :: Guard -> Board -> Board
insertGuardOnBoard (gpos, dir) (Board board) =
  let guardChar = case dir of
        North -> '^'
        East -> '>'
        West -> '<'
        South -> 'v'
  in
  Board $ insert gpos guardChar board

takeStep :: State BoardState Pos
takeStep = do
  _ <- updateGuardDirection
  (Board board, guard@(gpos, dir)) <- get
  let newBoard = insert gpos 'X' board
      newgPos = positionInFrontOfGuard guard
      newGuard = (newgPos, dir)
      newBoard' = insertGuardOnBoard newGuard (Board newBoard)
  put (newBoard', newGuard)
  return newgPos

willGuardLeaveBoard :: BoardState -> Bool
willGuardLeaveBoard (Board board, guard) =
  let nextgpos = positionInFrontOfGuard guard
      boardList = toList board
      maxX = maximum $ map (\((x, _), _) -> x) boardList
      maxY = maximum $ map (\((_, y), _) -> y) boardList
  in
  not $ nextgpos <= (maxX, maxY) && (0, 0) <= nextgpos

runGame :: BoardState -> (BoardState, Path)
runGame initialBoardState@(_, (initiagpos, _)) =
  let runGameInner boardState path =
        if willGuardLeaveBoard boardState then
          (boardState, path)
        else
          let (newGpos, newBoardState) = runState takeStep boardState in
          runGameInner newBoardState (path ++ [newGpos])
  in
    runGameInner initialBoardState [initiagpos]


isGuardChar :: Char -> Bool
isGuardChar c = c == '^' || c == '>' || c == '<' || c == 'v'

guardCharToDir :: Char -> Maybe Direction
guardCharToDir c = case c of
  '^' -> Just North
  '<' -> Just West
  '>' -> Just East
  'v' -> Just South
  _ -> Nothing

findGuard :: Board -> Maybe Guard
findGuard (Board board) = do
  (pos, c) <- find (\(_, c) -> isGuardChar c) (toList board)
  direction <- guardCharToDir c
  Just (pos, direction)

parseInput :: [[Char]] -> (Board, Guard)
parseInput input = 
  let rows_indexed = indexed input
      boardList = map (\(y, row) -> map (\(x, c) -> ((x, y), c)) (indexed row)) rows_indexed
      indexed_input = concat boardList
      board = Board $ fromList indexed_input
      guard = fromMaybe ((0,0), North) $ findGuard board
  in
    (board, guard)


main :: IO ()
main = do
  input <- readFile "input.txt"
  let (board, guard) = parseInput $ lines input
      initialBoardState = (board, guard) :: BoardState
      (finalBoardState, guardPath) = runGame initialBoardState
  print $ length $ nub guardPath
  print finalBoardState
