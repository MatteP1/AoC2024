module Main where

import Data.Tree ( unfoldForest, levels )
import Data.Map ( Map, empty, keys, insertWith, (!), elems, insert )

blink :: Int -> [Int]
blink stone
  | stone == 0 = [1]
  | even $ length $ show stone = 
    let stoneStr = show stone
        middle = length stoneStr `div` 2
        (left, right) = splitAt middle stoneStr  in 
        [read left, read right]
  | otherwise = [stone * 2024]

-- Part 1: Solved by generating Forest with unfoldForest and blink. Level n in the forest represents the line of stones after n blinks. For each tree in the forest, get level 25. Sum their lengths.
stonesAfterNBlinks :: [Int] -> Int -> [Int]
stonesAfterNBlinks initialStones n =
  let stoneForest = unfoldForest (\stone -> (stone, blink stone)) initialStones
      forestLevels = map (\tree -> levels tree) stoneForest
      forestLevelN = map (\tlevels -> tlevels !! n) forestLevels
      levelNStones = concat $ forestLevelN
    in 
      levelNStones

-- Part 2: Solution for part 1 scales very poorly, hence I came up with another solution for part 2, using 'stone counts'. A stone count maps each unique engraving to the number of stones in the list with that engraving.
-- E.g. the stone count for the list of stones [4, 2, 3, 4, 7, 2, 4] is (4 -> 3, 2 -> 2, 3 -> 1, 7 -> 1), as 3 stones are engraved with 4, 2 engraved with 2, 1 engraved with 3, and 1 engraved with 7.
type StoneCount = Map Int Int

-- Instead of explicitly listing the line of stones, we just calculate how the stone count changes each blink (which is much more efficient).
blinkStoneCount :: StoneCount -> StoneCount
blinkStoneCount stoneCount =
  let uniqueStones = keys stoneCount -- The current engravings as stones
      currStonesToNewStones = concatMap (\cs -> map (\ns -> (cs, ns)) $ blink cs) uniqueStones
      addPair (os, ns) sc = insertWith (+) ns (stoneCount ! os) sc
  in 
    foldr addPair empty currStonesToNewStones

runWithFuel :: (a -> a) -> a -> Int -> a
runWithFuel _ input 0 = input
runWithFuel f input fuel = runWithFuel f (f input) (fuel - 1)

countStonesFromStoneCount :: StoneCount -> Int
countStonesFromStoneCount sc = sum $ elems sc

main :: IO ()
main =
  let initialStones = [572556, 22, 0, 528, 4679021, 1, 10725, 2790]
      level25Stones = stonesAfterNBlinks initialStones 25

      initialStoneCount = foldr (\s sc -> insert s 1 sc) empty initialStones
      level75SC = runWithFuel blinkStoneCount initialStoneCount 75
  in do
    print $ length $ level25Stones -- part 1
    print $ countStonesFromStoneCount level75SC -- part 2
