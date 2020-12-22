module Day12
  ( day12,
  )
where

import Data.Foldable (foldl')
import Paths_aoc20 (getDataFileName)

data Action = N | S | E | W | L | R | F deriving (Eq, Show)

mkAction :: Char -> Action
mkAction 'N' = N
mkAction 'S' = S
mkAction 'E' = E
mkAction 'W' = W
mkAction 'L' = L
mkAction 'R' = R
mkAction 'F' = F

data Instruction = Instruction
  { iAction :: Action,
    iValue :: Integer
  }
  deriving (Show)

mkInstruction :: String -> Instruction
mkInstruction (action : value) = Instruction {iAction = mkAction action, iValue = read value}

data Facing = North | South | East | West deriving (Eq, Show)

faceLeft :: Facing -> Facing
faceLeft North = West
faceLeft South = East
faceLeft East = North
faceLeft West = South

faceRight :: Facing -> Facing
faceRight North = East
faceRight South = West
faceRight East = South
faceRight West = North

faceBack :: Facing -> Facing
faceBack North = South
faceBack South = North
faceBack East = West
faceBack West = East

data Ferry = Ferry
  { fX :: Integer,
    fY :: Integer,
    fFacing :: Facing
  }
  deriving (Show)

mkFerry :: Ferry
mkFerry = Ferry {fX = 0, fY = 0, fFacing = East}

data Ferry' = Ferry'
  { gX :: Integer,
    gY :: Integer,
    gWX :: Integer,
    gWY :: Integer
  }
  deriving (Show)

mkFerry' :: Ferry'
mkFerry' = Ferry' {gX = 0, gY = 0, gWX = 10, gWY = 1}

loadInput :: IO String
loadInput = getDataFileName "inputs/day-12.txt" >>= readFile

parseInput :: String -> [Instruction]
parseInput = map mkInstruction . lines

runInstruction :: Ferry -> Instruction -> Ferry
runInstruction ferry@Ferry {fX = x, fY = y, fFacing = facing} Instruction {iAction = action, iValue = value} =
  case action of
    N -> ferry {fY = y + value}
    S -> ferry {fY = y - value}
    E -> ferry {fX = x + value}
    W -> ferry {fX = x - value}
    L -> ferry {fFacing = turnLeft value facing}
    R -> ferry {fFacing = turnRight value facing}
    F -> runInstruction ferry (mkForwardInstruction facing value)
  where
    mkForwardInstruction facing value =
      let action = case facing of
            North -> N
            South -> S
            East -> E
            West -> W
       in Instruction {iAction = action, iValue = value}
    turnLeft 90 facing = faceLeft facing
    turnLeft 180 facing = faceBack facing
    turnLeft 270 facing = faceRight facing
    turnRight 90 facing = faceRight facing
    turnRight 180 facing = faceBack facing
    turnRight 270 facing = faceLeft facing

runInstruction' :: Ferry' -> Instruction -> Ferry'
runInstruction' ferry@Ferry' {gX = x, gY = y, gWX = wx, gWY = wy} Instruction {iAction = action, iValue = value} =
  case action of
    N -> ferry {gWY = wy + value}
    S -> ferry {gWY = wy - value}
    E -> ferry {gWX = wx + value}
    W -> ferry {gWX = wx - value}
    L -> ferry {gWX = fst $ turnLeft value (wx, wy), gWY = snd $ turnLeft value (wx, wy)}
    R -> ferry {gWX = fst $ turnRight value (wx, wy), gWY = snd $ turnRight value (wx, wy)}
    F -> ferry {gX = x + wx * value, gY = y + wy * value}
  where
    turnLeft 90 (x, y) = faceLeft (x, y)
    turnLeft 180 (x, y) = faceBack (x, y)
    turnLeft 270 (x, y) = faceRight (x, y)
    turnRight 90 (x, y) = faceRight (x, y)
    turnRight 180 (x, y) = faceBack (x, y)
    turnRight 270 (x, y) = faceLeft (x, y)
    faceLeft (x, y) = (- y, x)
    faceBack (x, y) = (- x, - y)
    faceRight (x, y) = (y, - x)

day12 :: IO ()
day12 = do
  instructions <- parseInput <$> loadInput
  let ferry = foldl' runInstruction mkFerry instructions
  print $ abs (fX ferry) + abs (fY ferry)
  let ferry' = foldl' runInstruction' mkFerry' instructions
  print $ abs (gX ferry') + abs (gY ferry')
