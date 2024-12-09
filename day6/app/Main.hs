module Main where

import Data.Map.Strict ( (!?), insert, Map, fromList, toAscList, toList, mapKeys )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find, nub)
import Data.List.Index
import Control.Monad.Trans.State
import Debug.Trace

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

takeStep :: State BoardState Guard
takeStep = do
  _ <- updateGuardDirection
  (Board board, guard@(gpos, dir)) <- get
  let newBoard = insert gpos 'X' board
      newgPos = positionInFrontOfGuard guard
      newGuard = (newgPos, dir)
      newBoard' = insertGuardOnBoard newGuard (Board newBoard)
  put (newBoard', newGuard)
  return newGuard

willGuardLeaveBoard :: BoardState -> Bool
willGuardLeaveBoard (Board board, guard) =
  let nextgpos = positionInFrontOfGuard guard
      boardList = toList board
      maxX = maximum $ map (\((x, _), _) -> x) boardList
      maxY = maximum $ map (\((_, y), _) -> y) boardList
  in
  not $ fst nextgpos <= maxX && snd nextgpos <= maxY && 0 <= fst nextgpos && 0 <= snd nextgpos

runGame :: BoardState -> (BoardState, Path)
runGame initialBoardState@(_, (initiagpos, _)) =
  let runGameInner boardState path =
        if willGuardLeaveBoard boardState then
          (boardState, path)
        else
          let (newGuard, newBoardState) = runState takeStep boardState in
          runGameInner newBoardState (path ++ [fst newGuard])
  in
    runGameInner initialBoardState [initiagpos]

runGameFindLoop :: BoardState -> (BoardState, [Guard], Bool)
runGameFindLoop initialBoardState@(_, initialGuard) =
  let runGameInner boardState guardPath
        | willGuardLeaveBoard boardState = (boardState, guardPath, False)
        | otherwise
        = let (newGuard, newBoardState) = runState takeStep boardState in
            if newGuard `elem` guardPath then (boardState, guardPath, True) else runGameInner newBoardState (guardPath ++ [newGuard])
  in
    runGameInner initialBoardState [initialGuard]


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
      (finalBoardState, guardStates, _) = runGameFindLoop initialBoardState

      boardTraversed = case finalBoardState of (Board b, _) -> b
      boardTraversedList = toList boardTraversed

      boardUnwrapped = case board of Board b -> b

      initialBoardStatesWithExtraObstacle = mapMaybe (\(pos, char) -> if (char == 'X') && pos /= (case guard of (gpos, _) -> gpos) then Just (Board $ insert pos '#' boardUnwrapped, guard) else Nothing) boardTraversedList

      loopingBoards = filter (\(gameId, ibs) ->
        let (_, _, looping) = runGameFindLoop $ traceShowWith (\bs -> (bs, gameId)) ibs in looping) $ indexed initialBoardStatesWithExtraObstacle

  print $ length $ nub $ map (\(gpos, _) -> gpos) guardStates -- part 1 (takes around 4 seconds to run)
  -- print finalBoardState
  print $ length initialBoardStatesWithExtraObstacle
  print $ length loopingBoards -- part 2 (takes around 3 hours to run)
