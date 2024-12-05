module Main where

import Data.Map.Strict ( (!?), mapAccumWithKey, Map )
import Data.Maybe (fromMaybe)

-- Strategy:
-- Store input as a map with keys being pairs (x,y) and values being letters. 
-- Create 8? functions to check each of the 8 directions from a point (x,y) for XMAS
-- If a check passes: +1
-- Do this for each point containing an 'X'

type Pos = (Int, Int)

type Board = Map Pos Char

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
  

main :: IO ()
main = putStrLn "Hello, Haskell!"
