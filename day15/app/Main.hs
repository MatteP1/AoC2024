module Main where

import Data.Map.Strict as Map ( Map, fromList, toList, insert, lookup )
import Data.Maybe (mapMaybe, fromJust)
import Data.List (find, sortOn, nub )
import Data.List.Index
import Control.Monad.State
    ( modify, execState, MonadState(get), State )
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

expand :: Char -> [Char]
expand c = case c of
  '#' -> "##"
  'O' -> "[]"
  '.' -> ".."
  '@' -> "@."
  _ -> [c]

preProcessInput :: [[Char]] -> [[Char]]
preProcessInput input = map (concatMap expand) input

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

getNextPos :: Pos -> Direction -> Pos
getNextPos fromPos direction = 
  case direction of
        North -> changeY fromPos (-1)
        East -> changeX fromPos (1)
        West -> changeX fromPos (-1)
        South -> changeY fromPos (1)

push :: Pos -> Direction -> Char -> State Board Pos
push fromPos direction object =
  let toPos = getNextPos fromPos direction
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

type Pushable = Bool

-- Could be somewhat nicer
findPiecesToPush :: Pos -> Direction -> Board -> Maybe [(Pos, Char)]
findPiecesToPush pos direction (Board board) = 
  let inner p =
        let pTypeMaybe = Map.lookup p board in
          case pTypeMaybe of
            Nothing -> Nothing
            Just toType ->
              case toType of
                '.' -> Just []
                '#' -> Nothing
                'O' -> do toPush <- inner $ getNextPos p direction
                          return ((p, 'O'):toPush)
                '[' -> if direction == North || direction == South then
                          do toPushL <- inner $ getNextPos p direction
                             toPushR <- inner $ getNextPos (changeX p 1) direction
                             return ((p, '['):(changeX p 1, ']'):toPushL++toPushR)
                        else
                            do toPush <- inner $ getNextPos p direction
                               return ((p, '['):toPush)
                ']' -> if direction == North || direction == South then
                          do toPushL <- inner $ getNextPos p direction
                             toPushR <- inner $ getNextPos (changeX p (-1)) direction
                             return ((p, ']'):(changeX p (-1), '['):toPushL++toPushR)
                        else
                            do toPush <- inner $ getNextPos p direction
                               return ((p, ']'):toPush)
                _ -> Nothing
    in
      fmap nub $ inner pos

forcePushPieces :: [(Pos, Char)] -> Direction -> Board -> Board
forcePushPieces pieces direction (Board board) = Board $
  let bRemovedPieces = foldr (\(p, _) b -> insert p '.' b) board pieces in
  foldr (\(p, c) b -> insert (getNextPos p direction) c b) bRemovedPieces pieces

push' :: Pos -> Direction -> Char -> State Board Pos
push' fromPos direction object =
  let toPos = getNextPos fromPos direction in
  get >>= \(Board board) ->
    let piecesToPushOpt = findPiecesToPush toPos direction (Board board) in
      case piecesToPushOpt of
        Nothing -> return fromPos
        Just piecesToPush ->
          modify (\b -> forcePushPieces piecesToPush direction b) >>= \_ ->
          modify (\(Board b) -> Board $ insert toPos object b) >>= \_ ->
          modify (\(Board b) -> Board $ insert fromPos '.' b) >>= \_ ->
          return toPos

runRobot :: Pos -> [Direction] -> State Board Path
runRobot startPos instructions =
  let inner :: Pos -> [Direction] -> Path -> State Board Path
      inner _ [] path = return path
      inner pos (d:ds) path =
        push' pos d '@' >>= \toPos -> inner toPos ds (toPos:path)
  in
    inner startPos instructions [] >>= \path -> return $ reverse path

posToGpsCoordinate :: Pos -> Int
posToGpsCoordinate (V2 x y) = 100 * y + x

findRobot :: Board -> Maybe Pos
findRobot (Board board) = do
  (pos, _) <- find (\(_, c) -> c == '@') $ toList board
  return pos

getBoxes :: Board -> [Pos]
getBoxes (Board board) = map fst $ filter (\(_, c) -> c == 'O') $ toList board 

getWideBoxes :: Board -> [Pos]
getWideBoxes (Board board) = map fst $ filter (\(_, c) -> c == '[') $ toList board 

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

      wideBoardInput = preProcessInput $ lines boardInput
      initialWideBoard = parseBoardInput wideBoardInput
      wideRobotPos = fromJust $ findRobot initialWideBoard

      finalWideBoard = execState (runRobot wideRobotPos directions) initialWideBoard

      wideBoxes = getWideBoxes finalWideBoard
      wideBoxesGpsCoords = map posToGpsCoordinate wideBoxes
    in
    do
      -- part 1
      print initialBoard
      print finalBoard
      print $ sum boxesGpsCoords
      
      -- part 2
      print initialWideBoard
      print finalWideBoard
      print $ sum wideBoxesGpsCoords
