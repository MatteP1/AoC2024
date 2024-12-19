module Main where

import Text.Megaparsec ( Parsec, parse )
import Text.Megaparsec.Char ( string, letterChar )

import Data.Void

import Data.Either (fromRight)

import Control.Monad.Combinators ( many, sepBy )
import Control.Monad.State
    ( modify, evalState, MonadState(get), State )
import Data.Maybe (mapMaybe)
import Text.Regex
import Data.List ( intersperse, isPrefixOf)
import Data.Map (Map, (!?), insert, empty)

type Parser = Parsec Void String

parsePatterns :: Parser [String]
parsePatterns = (many letterChar) `sepBy` (string ", ")

countWaysWithCache :: [String] -> String -> State (Map String Int) Int
countWaysWithCache patterns design =
  let inner :: String -> State (Map String Int) Int
      inner [] = return 1
      inner rest =
        get >>= \cache ->
          case cache !? rest of
            Just count -> return count
            Nothing ->
              let possibleNextPatterns = filter (`isPrefixOf` rest) patterns in
                  traverse (\pattern -> inner (drop (length pattern) rest)) possibleNextPatterns >>= \counts ->
                    let count = sum counts in
                      modify (insert rest count) >> return count
    in inner design

main :: IO ()
main = do
  patternsInput <- readFile "patterns.txt"
  designsInput <- readFile "designs.txt"
  let designs = lines designsInput
      patterns = fromRight [] $ parse parsePatterns "" patternsInput

      -- Part 1
      regexStrOr = (concat . intersperse "|") patterns
      regexStr = "^(" ++ regexStrOr ++ ")*$"
      rgx = mkRegex regexStr
      res = mapMaybe (matchRegex rgx) designs

      -- Part 2
      totalCount = sum $ evalState (traverse (countWaysWithCache patterns) designs) empty
    in do
      print $ length res
      print $ totalCount

