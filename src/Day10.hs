module Day10
  ( day10,
  )
where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-10.txt" >>= readFile

parseInput :: String -> [Integer]
parseInput = map read . words

joltDistribution :: [Integer] -> [(Integer, Integer)]
joltDistribution raw =
  let full = [0] ++ L.sort raw ++ [maximum raw + 3]
   in map bucket . L.group . L.sort $ L.zipWith (-) (L.drop 1 full) full
  where
    bucket xs = (L.head xs, L.genericLength xs)

arrangements :: [Integer] -> [[Integer]]
arrangements raw = go 0 (L.sort raw)
  where
    go last (a : []) = [[a]]
    go last (a : a' : as) =
      if last + 3 >= a'
        then ((a :) <$> go a (a' : as)) ++ go last (a' : as)
        else (a :) <$> go a (a' : as)

countArrangements :: [Integer] -> Integer
countArrangements raw = go 0 (L.sort raw)
  where
    go last (a : []) = 1
    go last (a : a' : as) =
      if last + 3 >= a'
        then go a (a' : as) + go last (a' : as)
        else go a (a' : as)

countArrangements' :: [Integer] -> Integer
countArrangements' raw = evalState (go 0 (L.sort raw)) M.empty
  where
    go :: Integer -> [Integer] -> State (M.Map (Integer, Integer) Integer) Integer
    go last (a : []) = return 1
    go last (a : a' : as) = do
      let key = (last, L.genericLength as)
      memo <- gets (M.lookup key)
      case memo of
        Just result -> return result
        Nothing -> do
          result <-
            if last + 3 >= a'
              then (+) <$> go a (a' : as) <*> go last (a' : as)
              else go a (a' : as)
          modify' (M.insert key result)
          return result

day10 :: IO ()
day10 = do
  raw <- parseInput <$> loadInput
  print $ product . map snd $ joltDistribution raw
  print $ countArrangements' raw
