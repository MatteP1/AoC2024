module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Maybe (mapMaybe)

-- This is basically a simplified number's game from countdown
-- https://github.com/MatteP1/Countdown_Solver/tree/main/lib/numbersgame/archived

-- An equation is a pair of goal and operands

type Operands = [Int]
type Operator = Int -> Int -> Int
type Operators = [Operator]
type Equation = (Int, Operands)

type Parser = Parsec Void String

parseEquation :: Parser Equation
parseEquation = do
  goal <- many numberChar
  _ <- char ':'
  _ <- char ' '
  operators <- sepBy (many numberChar) (char ' ')
  return (read goal, map (read) operators)

combineLeftMostNonDet :: Operands -> Operators -> [Operands]
combineLeftMostNonDet [] _ = [[]]
combineLeftMostNonDet [x] _ = [[x]]
combineLeftMostNonDet (x:y:ls) ops = map (\op -> (x `op` y : ls)) ops

computeNonDetStep :: [Operands] -> Operators -> [Operands]
computeNonDetStep computations ops =
  concatMap (\computation -> combineLeftMostNonDet computation ops) computations

-- computation is finished when nothing left to combine.
-- This corresponds to the fixed-point of computeNonDetStep
fix :: Eq t => (t -> t) -> t -> t
fix f x = let x' = f x in if x == x' then x else fix f x'

computeNonDet :: [Operands] -> Operators -> [Operands]
computeNonDet computations ops = fix (\l -> computeNonDetStep l ops) computations

computeNonDetStart :: Operands -> Operators -> [Int]
computeNonDetStart computations ops = concat $ computeNonDet [computations] ops

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputEquations = mapMaybe (parseMaybe parseEquation) $ lines input
      ops = [(+), (*)]
      validEquations = filter (\eq -> (fst eq) `elem` computeNonDetStart (snd eq) ops) inputEquations
      goalsSum = foldr (\eq acc -> (fst eq) + acc) 0 validEquations
    in
    print goalsSum