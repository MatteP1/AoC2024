module Main where

import Data.Tree ( unfoldForest, levels )

-- Generate Forest with unfoldForest. For each tree, get level 25. Sum their lengths

blink :: Int -> [Int]
blink stone
  | stone == 0 = [1]
  | even $ length $ show stone = 
    let stoneStr = show stone
        middle = length stoneStr `div` 2
        (left, right) = splitAt middle stoneStr  in 
        [read left, read right]
  | otherwise = [stone * 2024]

initialStones :: [Int]
initialStones = [572556, 22, 0, 528, 4679021, 1, 10725, 2790]

main :: IO ()
main =
  let stoneForest = unfoldForest (\stone -> (stone, blink stone)) initialStones
      forestLevels = map (\tree -> levels tree) stoneForest
      forestLevel25 = map (\tlevels -> tlevels !! 25) forestLevels
      level25stones = concat forestLevel25
  in
    print $ length level25stones
