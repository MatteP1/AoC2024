module Main where

import Data.List

isMonotone :: [Int] -> Bool
isMonotone [] = True
isMonotone (x:xs) = 
  let inner cmp = foldl' (\(prev, res) curr -> (curr, res && cmp curr prev)) (x, True) in
  snd (inner (<) xs) || snd (inner (>) xs)

isGradual :: [Int] -> Bool
isGradual =
  let isGradualInner res [] = res
      isGradualInner res [_] = res
      isGradualInner res (x:y:ls) = isGradualInner (res && abs (x - y) <= 3) (y:ls)
  in
    isGradualInner True

isSafe :: [Int] -> Bool
isSafe ls = isMonotone ls && isGradual ls

main :: IO ()
main = do
  input <- readFile "input.txt"
  let inputLines = lines input
      reports = map (map read . words) inputLines
  print (length $ filter isSafe reports)
