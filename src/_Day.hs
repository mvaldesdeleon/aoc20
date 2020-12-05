module DayXX
  ( dayXX,
  )
where

import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-XX.txt" >>= readFile

parseInput :: String -> String
parseInput = undefined

dayXX :: IO ()
dayXX = do
  input <- parseInput <$> loadInput
  print $ input
