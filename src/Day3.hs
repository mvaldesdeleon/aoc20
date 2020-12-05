module Day3
  ( day3,
  )
where

import Paths_aoc20 (getDataFileName)

slopes :: [(Integer, Integer)]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

loadInput :: IO String
loadInput = getDataFileName "inputs/day-3.txt" >>= readFile

parseInput :: String -> [String]
parseInput = map cycle . lines

countTrees :: [String] -> (Integer, Integer) -> Integer
countTrees forest (mx, my) = sum . map countTree $ zip [0 ..] forest
  where
    countTree (y, trees) =
      if y `mod` my == 0 && trees !! fI (y * mx) == '#' then 1 else 0

fI :: Num a => Integer -> a
fI = fromInteger

day3 :: IO ()
day3 = do
  forest <- parseInput <$> loadInput
  print $ countTrees forest (3, 1)
  print $ product $ map (countTrees forest) slopes
