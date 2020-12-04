module Day1
  ( day1,
  )
where

import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-1.txt" >>= readFile

day1 :: IO ()
day1 = do
  input <- loadInput
  print $ "hello world - " ++ input
