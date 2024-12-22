module Main where

import Data.Bits

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

getSN2000 :: Int -> Int
getSN2000 x = iterate nextSecretNumber x !! 2000

main :: IO ()
main = do
  input <- readFile "input.txt"
  let initialSecrets = map read $ lines input :: [Int]
      secretNumbers2000 = map getSN2000 initialSecrets
    in
    print $ sum secretNumbers2000


