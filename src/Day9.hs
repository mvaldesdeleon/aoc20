module Day9
  ( day9,
  )
where

import qualified Data.List as L
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

day9 :: IO ()
day9 = do
  raw <- parseInput <$> loadInput
  let xmas = mkXMAS 25 raw
      xs = iterateMaybe next xmas
  print $ L.head . xData . L.head $ L.dropWhile isValid xs
