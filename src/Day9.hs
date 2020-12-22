module Day9
  ( day9,
  )
where

import qualified Data.List as L
import qualified Data.Vector as V
import Paths_aoc20 (getDataFileName)

data XMAS = XMAS
  { xPreamble :: [Integer],
    xSums :: [[Integer]],
    xData :: [Integer]
  }
  deriving (Show)

mkXMAS :: Integer -> [Integer] -> XMAS
mkXMAS pSize raw =
  let (preamble, dta) = L.genericSplitAt pSize raw
   in XMAS {xPreamble = preamble, xSums = getSums preamble, xData = dta}
  where
    getSums [] = []
    getSums (p : ps) = ((p +) <$> ps) : getSums ps

next :: XMAS -> Maybe XMAS
next XMAS {xPreamble = preamble, xSums = sums, xData = dta} =
  case dta of
    [] -> Nothing
    (d : ds) -> Just $ XMAS {xPreamble = L.tail preamble ++ [d], xSums = updateSums preamble d sums, xData = ds}
  where
    updateSums (_ : ps) d (_ : ss) = L.zipWith (addSum d) ps ss ++ [[]]
    addSum d p s = s ++ [d + p]

isValid :: XMAS -> Bool
isValid XMAS {xSums = sums, xData = dta} =
  case dta of
    [] -> True
    (d : _) -> d `L.elem` L.concat sums

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : maybe [] (iterateMaybe f) (f a)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-9.txt" >>= readFile

parseInput :: String -> [Integer]
parseInput = map read . words

findWeakness :: Integer -> [Integer] -> Integer
findWeakness target raw = go 0 2
  where
    sums = V.fromList $ L.scanl (+) 0 raw
    go start end =
      let sum = sums V.! end - sums V.! start
       in case compare sum target of
            EQ -> compute $ L.take (end - start) . L.drop start $ raw
            LT -> go start (end + 1)
            GT -> go (start + 1) (start + 3)
    compute range = maximum range + minimum range

day9 :: IO ()
day9 = do
  raw <- parseInput <$> loadInput
  let xmas = mkXMAS 25 raw
      xs = iterateMaybe next xmas
      firstInvalid = L.head . xData . L.head $ L.dropWhile isValid xs
  print $ firstInvalid
  print $ findWeakness firstInvalid raw
