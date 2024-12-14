module Main where

import Data.Either

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Maybe (catMaybes)

data ClawMachine = ClawMachine { buttonA :: (Int, Int), buttonB :: (Int, Int), prize :: (Int, Int)}
  deriving (Show, Eq)

type Parser = Parsec Void String

parseInt :: Parser Int
parseInt = do
  n <- some numberChar
  return $ read n

parseClawMachine :: Parser ClawMachine
parseClawMachine = do
  _ <- string "Button A: X+"
  ax <- parseInt
  _ <- string ", Y+"
  ay <- parseInt
  _ <- newline
  _ <- string "Button B: X+"
  bx <- parseInt
  _ <- string ", Y+"
  by <- parseInt
  _ <- newline
  _ <- string "Prize: X="
  px <- parseInt
  _ <- string ", Y="
  py <- parseInt
  return ClawMachine {buttonA = (ax, ay), buttonB = (bx, by), prize = (px, py)}

parseClawMachines :: Parser [ClawMachine]
parseClawMachines = do
  parseClawMachine `sepBy` some newline

fixClawMachineConversionError :: ClawMachine -> ClawMachine
fixClawMachineConversionError ClawMachine {buttonA = a, buttonB = b, prize = (px, py)} =
  ClawMachine {buttonA = a, buttonB = b, prize = (10000000000000 + px, 10000000000000 + py)}


-- Models the problem as two equations with two unknowns
calculateTokenCost :: ClawMachine -> Maybe Int
calculateTokenCost ClawMachine {buttonA = (ax, ay), buttonB = (bx, by), prize = (px, py)} =
  let (a', r) = ((px * by - bx * py) `divMod` (ax * by - ay * bx)) 
      solution = if r == 0 then Just (a', (px - ax * a') `div` bx) else Nothing
  in do
    (a, b) <- solution
    return (a * 3 + b)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let clawMachines = fromRight [] $ parse parseClawMachines "" input
      -- part 1
      tokenCostsMaybe = map calculateTokenCost clawMachines

      tokenCosts = catMaybes tokenCostsMaybe
      tokenCostsSum = sum tokenCosts
      
      -- part 2
      fixedClawMachines = map fixClawMachineConversionError clawMachines
      tokenCostsFixedMaybe = map calculateTokenCost fixedClawMachines

      tokenCostsFixed = catMaybes tokenCostsFixedMaybe
      tokenCostsFixedSum = sum tokenCostsFixed

    in do
      print tokenCostsSum
      print tokenCostsFixedSum