module Main where

import Data.List.Index

type Page = Int
type Update = [Int]
type Rule = (Page, Page)
type Rules = [Rule]

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


main :: IO ()
main =
  let rules = [(1, 3), (3, 2), (4, 1)]
      updateValid = [4, 1, 1, 3, 2]
      updateViolation = [3, 4, 1]
  in
    do
    print $ fixUpdateOrder rules updateValid
    print $ fixUpdateOrder rules updateViolation
