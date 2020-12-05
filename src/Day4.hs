module Day4
  ( day4,
  )
where

import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-4.txt" >>= readFile

parseInput :: String -> String
parseInput = undefined

day4 :: IO ()
day4 = do
  input <- parseInput <$> loadInput
  print $ input
