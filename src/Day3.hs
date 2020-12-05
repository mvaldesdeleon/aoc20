module Day3
  ( day3,
  )
where

import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-3.txt" >>= readFile

parseInput :: String -> String
parseInput = undefined

day3 :: IO ()
day3 = do
  input <- parseInput <$> loadInput
  print $ input
