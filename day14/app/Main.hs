module Main where

import Control.Monad.Trans.State

import Text.Megaparsec (parse, Parsec, sepBy)
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Void (Void)
import Data.Either
import Data.List.Index
import Data.List (findIndex, sort)

type Pos = (Int, Int)
data Robot = Robot {pos :: Pos, vel :: (Int, Int)} 
  deriving (Eq, Show)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- signed space decimal
  return $ n

parseRobot :: Parser Robot
parseRobot = do
  _ <- string "p="
  px <- parseInt
  _ <- string ","
  py <- parseInt
  _ <- string " v="
  vx <- parseInt
  _ <- string ","
  vy <- parseInt
  return Robot {pos = (px, py), vel = (vx, vy)}

parseRobots :: Parser [Robot]
parseRobots = parseRobot `sepBy` newline

spaceX :: Int
spaceX = 101
spaceY :: Int
spaceY = 103

takeStep :: State Robot Pos
takeStep = do
  Robot {pos = (px, py), vel = (vx, vy)} <- get
  let newPos = ((px + vx) `mod` spaceX, (py + vy) `mod` spaceY)
  put $ Robot {pos = newPos, vel = (vx, vy)}
  return newPos

allRobotsStep :: [Robot] -> [Robot]
allRobotsStep robots = map (execState takeStep) robots

runWithFuel :: (a -> a) -> a -> Int -> a
runWithFuel _ input 0 = input
runWithFuel f input fuel = runWithFuel f (f input) (fuel - 1)

runWhile :: (a -> a) -> a -> (a -> Bool) -> (a, Int)
runWhile f input test = 
  let inner inp runs = if test inp then inner (f inp) (runs + 1) else (inp, runs)
  in inner input 0

putIntoFirstMatchingGroup :: (a -> a -> Bool) -> a -> [[a]]-> [[a]]
putIntoFirstMatchingGroup cmp a groups =
  let groupNumber = findIndex (\group -> case group of {[] -> False; (b:_) -> cmp a b}) groups
  in
    case groupNumber of
      Nothing -> [a]:groups
      Just i -> modifyAt i (a:) groups

detectBeam :: [Robot] -> Int -> Bool
detectBeam robots beamSize = 
  let xGroups = foldr (\(Robot {pos=p}) g -> putIntoFirstMatchingGroup (\p1 p2 -> fst p1 == fst p2) p g) [] robots
      yCoordsOrdered = map (\g -> map (snd) g) xGroups
      countLongestConsecutive list =
        let inner [] = 0
            inner (x:ls) = let (_, longest, _) = foldl (\(prev, best, curr) y -> if prev + 1 == y then (y, max (curr + 1) best, curr + 1) else (y, best, 0)) (x, 0, 0) ls
                           in longest
        in inner $ sort list
      beamsLength = map countLongestConsecutive yCoordsOrdered
  in
    or $ map (\l -> l >= beamSize) beamsLength

main :: IO ()
main = do
  input <- readFile "input.txt" 
  let robots = fromRight [] $ parse parseRobots "" input
      robotsAfter100 = runWithFuel allRobotsStep robots 100

      qCount cx cy = length $ filter (\Robot {pos = (px, py)} -> px `cx` (spaceX `div` 2) && py `cy` (spaceY `div` 2)) robotsAfter100

      q1Count = qCount (<) (<)
      q2Count = qCount (<) (>)
      q3Count = qCount (>) (<)
      q4Count = qCount (>) (>)

      christmasTreeBeamSize = spaceY `div` 8
      (_, runs) = runWhile allRobotsStep robots (\r -> not $ detectBeam r christmasTreeBeamSize)

    in do
      print $ q1Count * q2Count * q3Count * q4Count -- part 1
      print runs -- part 2
