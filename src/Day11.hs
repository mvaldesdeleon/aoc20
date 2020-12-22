{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Day11
  ( day11,
  )
where

import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Paths_aoc20 (getDataFileName)

data Cell = EmptySeat | BusySeat | Floor deriving (Eq)

instance Show Cell where
  show EmptySeat = "L"
  show BusySeat = "#"
  show Floor = "."

mkCell :: Char -> Cell
mkCell '.' = Floor
mkCell 'L' = EmptySeat

data Layout = Layout
  { lCells :: V.Vector Cell,
    lWidth :: Integer,
    lHeight :: Integer
  }
  deriving (Eq)

instance Show Layout where
  show Layout {lCells = cells, lWidth = width} =
    let cs = V.toList cells
     in unlines . sliceAt width $ (cs >>= show)
    where
      sliceAt _ [] = []
      sliceAt size list = L.genericTake size list : sliceAt size (L.genericDrop size list)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-11.txt" >>= readFile

parseInput :: String -> Layout
parseInput input =
  let rows = lines input
      height = length rows
      width = length . head $ rows
      cells = V.fromList . map mkCell $ concat rows
   in Layout {lCells = cells, lWidth = toInteger width, lHeight = toInteger height}

nextRound :: Layout -> Layout
nextRound layout@Layout {lCells = cells, lWidth = width, lHeight = height} =
  let nextCells = [nextCell x y | y <- [0 .. height - 1], x <- [0 .. width - 1]]
   in layout {lCells = V.fromList nextCells}
  where
    nextCell x y =
      case cells V.! fromInteger (y * width + x) of
        EmptySeat ->
          if occupiedAdjacent layout x y == 0
            then BusySeat
            else EmptySeat
        BusySeat ->
          if occupiedAdjacent layout x y >= 4
            then EmptySeat
            else BusySeat
        Floor -> Floor

occupiedAdjacent :: Layout -> Integer -> Integer -> Integer
occupiedAdjacent Layout {lCells = cells, lWidth = width, lHeight = height} x y =
  L.genericLength . filter (== BusySeat) $ mapMaybe lookupAdjacent [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  where
    lookupAdjacent (dx, dy) =
      let nx = x + dx
          ny = y + dy
       in if nx >= 0 && nx < width && ny >= 0 && ny < height
            then Just $ cells V.! fromInteger (ny * width + nx)
            else Nothing

nextRound' :: Layout -> Layout
nextRound' layout@Layout {lCells = cells, lWidth = width, lHeight = height} =
  let nextCells = [nextCell x y | y <- [0 .. height - 1], x <- [0 .. width - 1]]
   in layout {lCells = V.fromList nextCells}
  where
    nextCell x y =
      case cells V.! fromInteger (y * width + x) of
        EmptySeat ->
          if occupiedAdjacent' layout x y == 0
            then BusySeat
            else EmptySeat
        BusySeat ->
          if occupiedAdjacent' layout x y >= 5
            then EmptySeat
            else BusySeat
        Floor -> Floor

occupiedAdjacent' :: Layout -> Integer -> Integer -> Integer
occupiedAdjacent' Layout {lCells = cells, lWidth = width, lHeight = height} x y =
  L.genericLength . filter (== BusySeat) $ mapMaybe (lookupAdjacent x y) [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  where
    lookupAdjacent x y (dx, dy) =
      let nx = x + dx
          ny = y + dy
       in if nx >= 0 && nx < width && ny >= 0 && ny < height
            then case cells V.! fromInteger (ny * width + nx) of
              EmptySeat -> Just EmptySeat
              BusySeat -> Just BusySeat
              Floor -> lookupAdjacent nx ny (dx, dy)
            else Nothing

simulateUntilStable :: (Layout -> Layout) -> Layout -> Layout
simulateUntilStable next layout =
  let layouts = iterate next layout
   in findStable layouts
  where
    findStable (l : l' : ls) =
      if l == l'
        then l
        else findStable (l' : ls)

count :: Cell -> Layout -> Integer
count cell Layout {lCells = cells} = L.genericLength . filter (== cell) $ V.toList cells

day11 :: IO ()
day11 = do
  layout <- parseInput <$> loadInput
  print $ count BusySeat $ simulateUntilStable nextRound layout
  print $ count BusySeat $ simulateUntilStable nextRound' layout
