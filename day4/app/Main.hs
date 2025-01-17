module Main where

import Data.Map.Strict ( (!?), mapAccumWithKey, Map, fromList )
import Data.Maybe (fromMaybe)
import Data.List.Index

type Pos = (Int, Int)

type Board = Map Pos Char

parseInput :: [[Char]] -> Board
parseInput input = 
  let rows_indexed = indexed input
      board = map (\(y, row) -> map (\(x, c) -> ((x, y), c)) (indexed row)) rows_indexed
      indexed_input = concat board
  in
    fromList indexed_input

-- part 1
checkXMAS :: Board -> Pos -> Pos -> Pos -> Pos -> Bool
checkXMAS board p1 p2 p3 p4 =
  let word = do
        c1 <- board !? p1
        c2 <- board !? p2
        c3 <- board !? p3
        c4 <- board !? p4
        return ([c1, c2, c3, c4] == "XMAS")
  in
    fromMaybe False word

changeX :: Pos -> Int -> Pos
changeX pos delta = (fst pos + delta, snd pos)

changeY :: Pos -> Int -> Pos
changeY pos delta = (fst pos, snd pos + delta)

changeXY :: Pos -> Int -> Int -> Pos
changeXY pos deltaX = changeY (changeX pos deltaX)

generateDirectionPositions :: Pos -> Int -> Int -> (Pos, Pos, Pos, Pos)
generateDirectionPositions pos deltaX deltaY = 
  let p1 = pos
      p2 = changeXY p1 deltaX deltaY
      p3 = changeXY p2 deltaX deltaY
      p4 = changeXY p3 deltaX deltaY
  in
    (p1, p2, p3, p4)

checkDirection :: Board -> Pos -> Int -> Int -> Bool
checkDirection board pos deltaX deltaY = 
  let (p1, p2, p3, p4) = generateDirectionPositions pos deltaX deltaY in
    checkXMAS board p1 p2 p3 p4

directions :: [(Int, Int)]
directions = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]

directionChecks :: [Board -> Pos -> Bool]
directionChecks = Prelude.map (\(deltaX, deltaY) board pos -> checkDirection board pos deltaX deltaY) directions

countMatchesPos :: Board -> Pos -> Int
countMatchesPos board pos = length $ Prelude.filter (\check -> check board pos) directionChecks

countMatches :: Board -> Int
countMatches board = fst (mapAccumWithKey (\acc pos c -> (acc + countMatchesPos board pos, c)) 0 board )
  
 -- part 2
check_X_MAS :: Board -> Pos -> Bool
check_X_MAS board pC =
  let pNW = changeXY pC (-1) 1
      pSE = changeXY pC 1 (-1)
      pNE = changeXY pC 1 1
      pSW = changeXY pC (-1) (-1)
      
      word1MAS = do
        cC <- board !? pC
        cNW <- board !? pNW
        cSE <- board !? pSE
        return ([cNW, cC, cSE] == "MAS" || [cSE, cC, cNW] == "MAS")
      
      word2MAS = do
        cC <- board !? pC
        cNE <- board !? pNE
        cSW <- board !? pSW
        return ([cNE, cC, cSW] == "MAS" || [cSW, cC, cNE] == "MAS")
  in
    fromMaybe False word1MAS && fromMaybe False word2MAS

countXMatches :: Board -> Int
countXMatches board = fst (mapAccumWithKey (\acc pos c -> (if check_X_MAS board pos then acc + 1 else acc, c)) 0 board )

main :: IO ()
main = do
  input <- readFile "input.txt"
  let board = parseInput $ lines input
  print $ countMatches board -- part 1
  print $ countXMatches board -- part 2
