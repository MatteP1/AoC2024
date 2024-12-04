module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type MulInstr = (Int, Int)

type Parser = Parsec Void String

parseNum :: Parser Int
parseNum = do
  num <- count' 1 3 digitChar
  return (read num)

parseMul :: Parser String
parseMul = string "Mul"

parseMulInstr :: Parser MulInstr
parseMulInstr = do
  _ <- parseMul
  _ <- char '('
  n1 <- parseNum
  _ <- char ','
  n2 <- parseNum
  _ <- char ')'
  return (n1, n2)

-- Todo: finish
-- https://markkarpov.com/tutorial/megaparsec.html#working-with-alternatives
parseFile :: Parser [MulInstr]
parseFile = many (parseMulInstr)

-- exec :: [MulInstr] -> Int
-- exec instrs = sum $ map (\(a, b) -> a * b) instrs

main :: IO ()
main = do
  input <- readFile "input.txt"
  parseTest parseMulInstr "Mul(1,333)"
