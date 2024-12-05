module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type MulInstr = (Int, Int)

type Prog = [MulInstr]

type Parser = Parsec Void String

parseNum :: Parser Int
parseNum = do
  num <- count' 1 3 digitChar
  return (read num)

parseMul :: Parser String
parseMul = string "mul"

parseMulInstr :: Parser MulInstr
parseMulInstr = do
  _ <- parseMul
  _ <- char '('
  n1 <- parseNum
  _ <- char ','
  n2 <- parseNum
  _ <- char ')'
  return (n1, n2)

parseFile :: Parser Prog
parseFile = many (try $ skipManyTill anySingle (try parseMulInstr))

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseErrorBundle String e) a)
parseFromFile p file = runParser p file <$> readFile file

exec :: Prog -> Int
exec instrs = sum $ map (\(a, b) -> a * b) instrs

main :: IO ()
main = do
  parsed <- parseFromFile parseFile "input.txt"
  case parsed of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> print $ exec xs
