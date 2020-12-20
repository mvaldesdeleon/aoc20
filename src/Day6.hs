module Day6
  ( day6,
  )
where

import qualified Data.List as L
import qualified Data.List.Split as S
import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-6.txt" >>= readFile

parseInput :: String -> [[String]]
parseInput = map (filter (not . null) . S.splitOn "\n") . S.splitOn "\n\n"

uniqueQuestions :: [String] -> Integer
uniqueQuestions = L.genericLength . L.group . L.sort . concat

commonQuestions :: [String] -> Integer
commonQuestions group = L.genericLength . filter ((== length group) . length) . L.group . L.sort . concat $ group

day6 :: IO ()
day6 = do
  groups <- parseInput <$> loadInput
  print $ sum . map uniqueQuestions $ groups
  print groups
  print $ sum . map commonQuestions $ groups
