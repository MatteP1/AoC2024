module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type MulInstr = (Int, Int)

type Prog = [MulInstr]

type Parser = Parsec Void String

-- part 1
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

parseProg :: Parser Prog
parseProg = many (try $ skipManyTill anySingle (try parseMulInstr))

-- part 2
parseDo :: Parser String
parseDo = string "do()"

parseDont :: Parser String
parseDont = string "don't()"

parseNextDo :: Parser String
parseNextDo = skipManyTill anySingle parseDo

parseNextDont :: Parser String
parseNextDont = manyTill anySingle parseDont

parsePreprocess :: Parser String
parsePreprocess =
  do
    filteredInput <- many (try $ parseNextDont <* parseNextDo)
    rest <- parseNextDont <* takeRest
    return $ concat filteredInput ++ rest

parseFromFile :: Parsec e String a -> String -> IO (Either (ParseErrorBundle String e) a)
parseFromFile p file = runParser p file <$> readFile file

exec :: Prog -> Int
exec instrs = sum $ map (\(a, b) -> a * b) instrs

main :: IO ()
main =
  do
    -- part 1
    parsed1 <- parseFromFile parseProg "input.txt"
    case parsed1 of
      Left bundle -> putStr (errorBundlePretty bundle)
      Right xs -> print $ exec xs
    
    -- part 2
    parsed2 <- parseFromFile parsePreprocess "input.txt"
    case parsed2 of
      Left bundle -> putStr (errorBundlePretty bundle)
      Right cleanedInput ->
        let prog = runParser parseProg "" cleanedInput in
        case prog of
          Left bundle -> putStr (errorBundlePretty bundle)
          Right xs -> print $ exec xs
