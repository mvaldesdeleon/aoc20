module Day1
  ( day1,
  )
where

import Data.List (nub)
import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-1.txt" >>= readFile

parseInput :: String -> [Integer]
parseInput = map read . lines

find2020 :: [Integer] -> Maybe (Integer, Integer)
find2020 [] = Nothing
find2020 (first : rest) =
  case filter (\second -> second + first == 2020) rest of
    [] -> find2020 rest
    (second : _) -> Just (first, second)

find2020' :: [Integer] -> Maybe (Integer, Integer, Integer)
find2020' nums =
  case [(x, y, z) | x <- nums, y <- nums, z <- nums, x + y + z == 2020] of
    [] -> Nothing
    ((x, y, z) : _) -> Just (x, y, z)

day1 :: IO ()
day1 = do
  input <- parseInput <$> loadInput
  let Just (first, second) = find2020 input
  print $ first * second
  let Just (first', second', third') = find2020' input
  print $ first' * second' * third'
