module Main where

import Data.Bits
import Linear.V4
import Data.Map as M (Map, alter, empty, elems)
import Data.Set as S (Set, empty, insert, notMember)
import Control.Monad.State ( State, modify, execState)
import Control.Monad (when)

mix :: Int -> Int -> Int
mix x y = x .^. y

-- 16777216 = 2^24, so we can just mask the result with 2^24 - 1
prune :: Int -> Int
prune x = x .&. 16777215

step1 :: Int -> Int
step1 x = prune $ mix (x .<<. 6) x

step2 :: Int -> Int
step2 x = prune $ mix (x .>>. 5) x

step3 :: Int -> Int
step3 x = prune $ mix (x .<<. 11) x

nextSecretNumber :: Int -> Int
nextSecretNumber = step3 . step2 . step1

getSecretNumbers :: Int -> [Int]
getSecretNumbers x = iterate nextSecretNumber x


type Sequence = V4 Int

findSequenceValues :: [(Int, Int)] -> State (Map Sequence Int) ()
findSequenceValues s =
  let inner :: Set Sequence -> [(Int, Int)] -> State (Map Sequence Int) ()
      inner done ((_, x1):(next@((_, x2):(_, x3):(sv, x4):_))) =
        let currSequence = V4 x1 x2 x3 x4 in
          do
            when (currSequence `notMember` done) $ modify (alter (\svMaybe -> case svMaybe of {Just sv' -> Just (sv' + sv); Nothing -> Just sv}) currSequence)
            inner (currSequence `insert` done) next
      inner _ _ = return ()
  in
    inner S.empty s


main :: IO ()
main = do
  input <- readFile "input.txt"
  let initialSecrets = map read $ lines input :: [Int]
      secretNumbers = map ((take 2001) . getSecretNumbers) initialSecrets

      secretNumbers2000 = map last secretNumbers

      allCosts = map (\secnums -> map (`mod` 10) secnums) secretNumbers

      changes = map (\costs -> zipWith (-) (drop 1 costs) costs) allCosts

      allCostChanges = map (\(p1, p2) -> zip p1 p2) $ zip (map (drop 1) allCosts) changes

      sequenceValues = execState (traverse findSequenceValues allCostChanges) M.empty

    in do
      print $ sum secretNumbers2000
      print $ maximum (elems sequenceValues) -- takes ~10 seconds


