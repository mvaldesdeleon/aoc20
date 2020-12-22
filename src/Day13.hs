module Day13
  ( day13,
  )
where

import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.Split as S
import Data.Maybe (catMaybes)
import Paths_aoc20 (getDataFileName)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-13.txt" >>= readFile

data Input = Input
  { iTimestamp :: Integer,
    iBuses :: [Maybe Integer]
  }
  deriving (Show)

parseInput :: String -> Input
parseInput input =
  let [first, second] = lines input
      buses = S.splitOn "," second
   in Input {iTimestamp = read first, iBuses = map readBus buses}
  where
    readBus "x" = Nothing
    readBus id = Just $ read id

nextBus :: Input -> (Integer, Integer)
nextBus Input {iTimestamp = timestamp, iBuses = buses} =
  let ids = catMaybes buses
   in L.minimumBy (compare `on` snd) $ map minutesLeft ids
  where
    minutesLeft id = (id, id - (timestamp `mod` id))

busAndDeltas :: Input -> [(Integer, Integer)]
busAndDeltas Input {iTimestamp = timestamp, iBuses = buses} = catPairs $ zip buses [0 ..]
  where
    catPairs [] = []
    catPairs ((Nothing, _) : rest) = catPairs rest
    catPairs ((Just id, idx) : rest) = (id, idx) : catPairs rest

xxx :: Input -> Integer
xxx input =
  let bad = busAndDeltas input
      (id, delta) = L.maximumBy (compare `on` fst) bad
   in go bad id delta (100000000000000 `div` id)
  where
    go bad id delta n =
      let t = n * id - delta
       in if match bad t
            then t
            else go bad id delta (n + 1)
    match bad t = all (\(id, delta) -> (t + delta) `mod` id == 0) bad

day13 :: IO ()
day13 = do
  input <- parseInput <$> loadInput
  print $ uncurry (*) $ nextBus input
  print $ xxx input
