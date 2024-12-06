module Main where

import Data.List.Index
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.List (partition)

type Page = Int
type Update = [Int]
type Rule = (Page, Page)
type Rules = [Rule]

type Parser = Parsec Void String

parsePage :: Parser Page
parsePage = do
  page <- many numberChar
  return $ read page

parseRule :: Parser Rule
parseRule = do
  page1 <- parsePage
  _ <- char '|'
  page2 <- parsePage
  return (page1, page2)

parseUpdate :: Parser Update
parseUpdate = sepBy parsePage (char ',')

findViolation :: Rules -> Page -> Update -> Maybe (Int, Page)
findViolation rules page = ifind (\_ page' -> (page', page) `elem` rules)

fixUpdateOrder :: Rules -> Update -> Update
fixUpdateOrder rules update =
  let fixUpdateOrderInner fixed [] = fixed
      fixUpdateOrderInner fixed (page:toFix) = 
        case findViolation rules page toFix of
          Nothing -> fixUpdateOrderInner (fixed ++ [page]) toFix
          Just (index, page') ->
            fixUpdateOrderInner fixed (page':setAt index page toFix)
  in
    fixUpdateOrderInner [] update

partitionUpdatesOnCorrectness :: Rules -> [Update] -> ([Update], [Update])
partitionUpdatesOnCorrectness rules = partition (\update -> update == fixUpdateOrder rules update)

middle :: [a] -> Maybe a
middle []          = Nothing
middle [x]         = Just x
middle [x, _]      = Just x
middle l@(_:_:_:_) = middle $ tail $ init l


main :: IO ()
main = do
    rulesInput <- readFile "rules.txt"
    let rules = mapMaybe (parseMaybe parseRule) (lines rulesInput)
    updatesInput <- readFile "updates.txt"
    let updates = mapMaybe (parseMaybe parseUpdate) (lines updatesInput)
        (correctUpdates, incorrectUpdates) = partitionUpdatesOnCorrectness rules updates
  
    -- part 1
    print $ sum $ mapMaybe middle correctUpdates
    
    -- part 2
    print $ sum $ mapMaybe (middle . fixUpdateOrder rules) incorrectUpdates
