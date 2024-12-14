module Main where

import Prelude hiding (Num(..))

import Algebra.Classes
import Control.Monad.LPMonad
import Data.LinearProgram.Common
import Data.LinearProgram

import Data.Either

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Maybe (mapMaybe)

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

-- Problem is modelled as an Integer Linear Programming problem

generateILPFromClawMachine :: ClawMachine -> LP String Int
generateILPFromClawMachine ClawMachine {buttonA = (ax, ay), buttonB = (bx, by), prize = (px, py)} = 
  let objFun = linCombination [(3, "A"), (1, "B")] in
    execLPM $ do
    setDirection Min
    setObjective objFun
    equalTo (ax *& "A" + bx *& "B") px -- X
    equalTo (ay *& "A" + by *& "B") py -- Y
    varGeq "A" 0
    varGeq "B" 0
    setVarKind "A" IntVar
    setVarKind "B" IntVar

(*&) :: (Ord v, Additive r) => r -> v -> LinFunc v r
n *& v = linCombination [(n,v)]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let clawMachines = fromRight [] $ parse parseClawMachines "" input 
      results = mapM (glpSolveVars mipDefaults . generateILPFromClawMachine) clawMachines

      tokenCostsAndButtons = fmap (mapMaybe snd) results
      tokenCostsAndButtonsAggregated = fmap unzip tokenCostsAndButtons
      tokenCosts = fmap (Prelude.sum . fst) tokenCostsAndButtonsAggregated
    in do
      print "hi"
      print =<< tokenCosts