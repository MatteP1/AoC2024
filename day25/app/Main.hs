module Main where

import Text.Megaparsec ( Parsec, parse, (<|>), sepBy, some, sepEndBy)
import Text.Megaparsec.Char (newline, char )

import Data.Void (Void)

import Data.List (transpose, findIndex)
import Data.Maybe (mapMaybe)
import Data.Either (fromRight, partitionEithers)

type Parser = Parsec Void String

type Lock = [Int]

type Key = [Int]

type Component = Either Key Lock

height :: Int
height = 7

parseKey :: [[Char]] -> Key
parseKey input =
  let it = transpose input
      itm = mapMaybe (findIndex (\c -> c == '#')) it
      ith = map (\i -> height - i) itm
  in
    ith

parseLock :: [[Char]] -> Lock
parseLock input =
  let it = transpose input
      itm = mapMaybe (findIndex (\c -> c == '.')) it
  in
    itm

isLock :: [[Char]] -> Bool
isLock input = all (== '#') (head input)

parseComponent :: Parser Component
parseComponent = do
  comp <- (some (char '.' <|> char '#' )) `sepEndBy` newline
  if isLock comp then
    return $ Right $ parseLock comp
  else
    return $ Left $ parseKey comp

parseComponents :: Parser [Component]
parseComponents = parseComponent `sepBy` newline

-- Could return earlier by trying to find a collision instead
isKeyAndLockCompatible :: Key -> Lock -> Bool
isKeyAndLockCompatible key lock =
  let fits = zipWith (\kl ll -> height >= kl + ll) key lock
  in all id fits

main :: IO ()
main = do
  input <- readFile "input.txt"
  let components = fromRight [] $ parse parseComponents "" input
      (keys, locks) = partitionEithers components
      allPairs = [(k, l) | k <- keys, l <- locks]
      allCompatiblePairs = filter (\(k, l) -> isKeyAndLockCompatible k l) allPairs
    in
      print $ length allCompatiblePairs