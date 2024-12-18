module Main where

import Algorithm.Search
import Linear.V2
import Data.List ((\\))
import Data.Void (Void)

import Text.Megaparsec (parse, Parsec, sepBy)
import Text.Megaparsec.Char (newline, string, space)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Either (fromRight)
import Data.Maybe (fromJust)

type Parser = Parsec Void String

type Pos = V2 Int

type Corrupted = [Pos]

type State = (Pos, Corrupted)

parseInt :: Parser Int
parseInt = do
  n <- signed space decimal
  return $ n

parseCoord :: Parser Pos
parseCoord = do
  px <- parseInt
  _ <- string ","
  py <- parseInt
  return $ V2 px py

parseCoords :: Parser [Pos]
parseCoords = parseCoord `sepBy` newline

next :: State -> [State]
next (V2 x y, corrupted) =
  let neighbours = [V2 (x + 1) y, V2 (x - 1) y, V2 x (y + 1), V2 x (y - 1)]
      inBounds = filter (\(V2 xf yf) -> xf <= 70 && yf <= 70 && xf >= 0 && yf >= 0) neighbours
      nonCorrupted = inBounds \\ corrupted
    in
      map (\p -> (p, corrupted)) nonCorrupted

found :: State -> Bool
found (p, _) = p == V2 70 70

initialState :: Corrupted -> State
initialState corrupted = (V2 0 0, corrupted)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let corrupted = fromRight [] $ parse parseCoords "" input
      corruptedKb = take 1024 corrupted
      is = initialState corruptedKb
      result = bfs next found is
    in
    print $ length $ fromJust result
