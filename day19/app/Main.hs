module Main where

import Text.Megaparsec ( Parsec, parse )
import Text.Megaparsec.Char ( string, letterChar )

import Data.Void

import Data.Either (fromRight)

import Control.Monad.Combinators ( many, sepBy )
import Data.Maybe (mapMaybe)
import Text.Regex
import Data.List ( intersperse )

type Parser = Parsec Void String

parsePatterns :: Parser [String]
parsePatterns = (many letterChar) `sepBy` (string ", ")

main :: IO ()
main = do
  patternsInput <- readFile "patterns.txt"
  designsInput <- readFile "designs.txt"
  let designs = lines designsInput
      patterns = fromRight [] $ parse parsePatterns "" patternsInput

      regexStrOr = (concat . intersperse "|") patterns

      regexStr = "^(" ++ regexStrOr ++ ")*$"

      rgx = mkRegex regexStr
      
      res = mapMaybe (matchRegex rgx) designs
    in
      print $ length res

