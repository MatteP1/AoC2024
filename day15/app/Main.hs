module Main where

import Data.Map.Strict as Map ( Map, fromList, toAscList, toList, mapKeys, insert, lookup )
import Data.Maybe (mapMaybe, fromJust)
import Data.List (find)
import Data.List.Index
import Control.Monad.State
import Linear.V2

type AntennaType = Char

type Pos = V2 Int

data Direction =
    North
  | West
  | East
  | South
    deriving (Eq, Show)

type Path = [Pos]

newtype Board = Board (Map Pos Char)

showBoard :: Board -> String
showBoard (Board board) =
  let boardList = toAscList $ mapKeys (\(V2 x y)  -> (y,x)) board
      boardSize = maximum $ map (\((x, _), _) -> x) boardList
  in
    foldl (\acc ((_, y), c) -> acc ++ [c] ++ if y == boardSize then "\n" else "") "" boardList

instance Show Board where
  show board = showBoard board

parseBoardInput :: [[Char]] -> Board
parseBoardInput input =
  let rows_indexed = indexed input
      boardList = map (\(y, row) -> map (\(x, c) -> ((V2 x y), c)) (indexed row)) rows_indexed
      indexed_input = concat boardList
  in
    Board $ fromList indexed_input

parseDirectionsInput :: [Char] -> [Direction]
parseDirectionsInput input =
  mapMaybe (\c -> case c of
                    '^' -> Just North
                    '>' -> Just East
                    '<' -> Just West
                    'v' -> Just South
                    _   -> Nothing) input


changeX :: Pos -> Int -> Pos
changeX pos delta = pos + V2 delta 0

changeY :: Pos -> Int -> Pos
changeY pos delta = pos + V2 0 delta

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)



push :: Pos -> Direction -> Char -> State Board Pos
push fromPos direction object =
  let toPos = case direction of
                North -> changeY fromPos (-1)
                East -> changeX fromPos (1)
                West -> changeX fromPos (-1)
                South -> changeY fromPos (1)
  in
    get >>= \(Board board) ->
      let toTypeMaybe = Map.lookup toPos board in
        case toTypeMaybe of
          Nothing -> return fromPos
          Just toType ->
            case toType of
              '.' -> modify (\(Board b) -> Board (insert toPos object b)) >>= \_ ->
                     modify (\(Board b) -> Board (insert fromPos '.' b)) >>= \_ ->
                     return toPos
              '#' -> return fromPos
              'O' -> push toPos direction 'O' >>= \toPos' ->
                     if toPos == toPos' then return fromPos
                     else modify (\(Board b) -> Board (insert toPos object b)) >>= \_ ->
                          modify (\(Board b) -> Board (insert fromPos '.' b)) >>= \_ ->
                          return toPos
              _ -> return fromPos

runRobot :: Pos -> [Direction] -> State Board Path
runRobot startPos instructions =
  let inner :: Pos -> [Direction] -> Path -> State Board Path
      inner _ [] path = return path
      inner pos (d:ds) path =
        push pos d '@' >>= \toPos -> inner toPos ds (toPos:path)
  in
    inner startPos instructions [] >>= \path -> return $ reverse path

posToGpsCoordinate :: Pos -> Int
posToGpsCoordinate (V2 x y) = 100 * y + x

findRobot :: Board -> Maybe Pos
findRobot (Board board) = do
  (pos, _) <- find (\(_, c) -> c == '@') $ toList board
  return pos

getBoxes :: Board -> [Pos]
getBoxes (Board board)= map fst $ filter (\(_, c) -> c == 'O') $ toList board 

main :: IO ()
main = do
  boardInput <- readFile "board.txt"
  directionsInput <- readFile "directions.txt"
  let initialBoard = parseBoardInput $ lines boardInput
      directions = parseDirectionsInput directionsInput
      robotPos = fromJust $ findRobot initialBoard

      finalBoard = execState (runRobot robotPos directions) initialBoard

      boxes = getBoxes finalBoard
      boxesGpsCoords = map posToGpsCoordinate boxes
    in
    do
      print initialBoard
      print finalBoard
      print $ sum boxesGpsCoords
