module Day5
  ( day5,
  )
where

import qualified Data.List as L
import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-5.txt" >>= readFile

parseInput :: String -> [String]
parseInput = words

seatID :: String -> Integer
seatID = go . reverse
  where
    go [] = 0
    go (d : ds) =
      case d of
        'F' -> 0 + 2 * go ds
        'B' -> 1 + 2 * go ds
        'R' -> 1 + 2 * go ds
        'L' -> 0 + 2 * go ds

searchSeat :: [Integer] -> Integer
searchSeat (a : b : rest) =
  if b == a + 2
    then a + 1
    else searchSeat (b : rest)

day5 :: IO ()
day5 = do
  codes <- map seatID . parseInput <$> loadInput
  print $ maximum codes
  print $ searchSeat . L.sort $ codes
