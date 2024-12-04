module Main where

import Data.List

-- Part 1
isSafe :: [Int] -> Bool
isSafe [] = True
isSafe (x:xs) =
  let check cmp l1 l2 = cmp l1 l2 && abs (l1 - l2) <= 3
      inner cmp = foldl' (\(prev, res) curr -> (curr, res && check cmp curr prev)) (x, True) in
  snd (inner (<) xs) || snd (inner (>) xs)

-- Part 2: Try all ways of removing one element. See if at least one is safe.
-- Note: we don't need to check if the entire report is safe. If the entire report is safe, then a report with the first/last level removed will also be safe.
takeOneNonDet :: [a] -> [[a]]
takeOneNonDet [] = [[]]
takeOneNonDet (x:xs) = xs : map (x:) (takeOneNonDet xs)

isSafeWithDampener :: [Int] -> Bool
isSafeWithDampener xs = or (map isSafe (takeOneNonDet xs))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputLines = lines input
      reports = map (map read . words) inputLines
  print (length $ filter isSafe reports) -- Part 1: 390
  print (length $ filter isSafeWithDampener reports) -- Part 2: 439

