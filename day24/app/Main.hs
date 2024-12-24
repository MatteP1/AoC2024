module Main where

import Data.Map as M ( lookup, fromList, insert, Map, empty, toList )
import Control.Monad.State

import Text.Megaparsec ( Parsec, parse, many, (<|>), sepBy)
import Text.Megaparsec.Char ( string, space, alphaNumChar, newline )

import Data.Void
import Data.Either (fromRight)
import Data.Maybe (catMaybes)
import Data.List (sort)

data BinOp = AND | OR | XOR
  deriving (Show)

type Wires = Map String Bool

data Gate = Gate {
  wire1 :: String,
  gateType :: BinOp,
  wire2 :: String,
  wireOut :: String
}
  deriving (Show)

type Parser = Parsec Void String

parseGateOP :: Parser BinOp
parseGateOP = do
  (string "AND" >> return AND)
  <|> (string "OR" >> return OR)
  <|> (string "XOR" >> return XOR)

parseWireName :: Parser String
parseWireName = many alphaNumChar

parseGate :: Parser Gate
parseGate = do
  w1 <- parseWireName
  _ <- space
  bop <- parseGateOP
  _ <- space
  w2 <- parseWireName
  _ <- string " -> "
  wo <- parseWireName
  return $ Gate w1 bop w2 wo

parseGates :: Parser [Gate]
parseGates = parseGate `sepBy` newline

parseWire :: Parser (String, Bool)
parseWire = do
  w <- parseWireName
  _ <- string ": "
  b <- (string "1" >> return True) <|> (string "0" >> return False)
  return (w, b)

parseWires :: Parser Wires
parseWires = do
  wires <- parseWire `sepBy` newline
  return $ fromList wires


eval :: Gate -> State (Wires) Bool
eval (Gate w1 bop w2 wo) = do
  wires <- get
  let v1 = M.lookup w1 wires
      v2 = M.lookup w2 wires
      output = 
        case bop of
          AND ->
            case (v1, v2) of
            (Just True, Just True) -> Just True
            (Just False, _) -> Just False
            (_, Just False) -> Just False
            _ -> Nothing
          OR ->
            case (v1, v2) of
            (Just True, _) -> Just True
            (_, Just True) -> Just True
            (Just False, Just False) -> Just False
            _ -> Nothing
          XOR ->
            case (v1, v2) of
            (Just True, Just False) -> Just True
            (Just False, Just True) -> Just True
            (Just True, Just True) -> Just False
            (Just False, Just False) -> Just False
            _ -> Nothing
    in
      case output of
        Nothing -> return False
        Just woRes -> modify (insert wo woRes) >> return True

evalContinuously :: [Gate] -> State (Wires) [Gate]
evalContinuously [] = return []
evalContinuously gates = 
  let gatesOpt = traverse (\gate -> do {s <- eval gate; return $ if s then Nothing else Just gate}) gates
    in do
      opts <- gatesOpt
      evalContinuously $ catMaybes opts
  
convert :: [Bool] -> Int
convert bs = sum $ zipWith (\b i -> if b then 2^i else 0) bs [0..]


main :: IO ()
main = do
  wiresInput <- readFile "wireInputs.txt"
  gatesInput <- readFile "gates.txt"
  let
      initialWires = fromRight M.empty $ parse parseWires "" wiresInput

      gates = fromRight [] $ parse parseGates "" gatesInput

      finalWires = execState (evalContinuously gates) initialWires

      fwList = toList finalWires

      zWires = filter (\(wname, _) -> head wname == 'z') fwList

      zWiresSorted = sort zWires

      res = convert $ map snd zWiresSorted


   in do
    print res
