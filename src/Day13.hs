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

solveContest :: Input -> Integer
solveContest input =
  let congruenceSystem = map xx $ busAndDeltas input
   in fst $ crt congruenceSystem
  where
    xx (bus, delta) = (if delta > 0 then bus - delta else 0, bus)

-- Shamelessly stolen from StackOverflow: https://stackoverflow.com/a/35529381
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
  where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a

day13 :: IO ()
day13 = do
  input <- parseInput <$> loadInput
  print $ uncurry (*) $ nextBus input
  print $ solveContest input
