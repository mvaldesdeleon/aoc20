module Day8
  ( day8,
  )
where

import Data.Functor (($>))
import qualified Data.List as L
import qualified Data.Vector as V
import Paths_aoc20 (getDataFileName)
import qualified Text.Parsec as P

data OpCode = ACC | JMP | NOP
  deriving (Show, Eq)

data Instruction = Instruction
  { iOpCode :: OpCode,
    iArg :: Integer
  }
  deriving (Show)

newtype Program = Program (V.Vector Instruction)
  deriving (Show)

data Computer = Computer
  { cProgram :: Program,
    cIp :: Integer,
    cAcc :: Integer
  }
  deriving (Show)

mkComputer :: Program -> Computer
mkComputer prg = Computer {cProgram = prg, cIp = 0, cAcc = 0}

runStep :: Computer -> Computer
runStep computer@Computer {cProgram = Program insts, cIp = ip} =
  case insts V.!? fromInteger ip of
    Just inst -> runInstruction inst computer
    Nothing -> error $ "Instruction pointer out of bounds: " ++ show ip

runInstruction :: Instruction -> Computer -> Computer
runInstruction Instruction {iOpCode = opCode, iArg = arg} computer@Computer {cIp = ip, cAcc = acc} =
  case opCode of
    ACC -> computer {cIp = ip + 1, cAcc = acc + arg}
    JMP -> computer {cIp = ip + arg}
    NOP -> computer {cIp = ip + 1}

loadInput :: IO String
loadInput = getDataFileName "inputs/day-8.txt" >>= readFile

parseOpCode :: P.Parsec String () OpCode
parseOpCode =
  P.try (P.string "acc" $> ACC)
    P.<|> P.try (P.string "jmp" $> JMP)
    P.<|> P.try (P.string "nop" $> NOP)
    P.<?> "OpCode"

parseNumber :: P.Parsec String () Integer
parseNumber = do
  sign <- (P.char '+' $> 1) P.<|> (P.char '-' $> -1)
  number <- read <$> P.many1 P.digit
  return $ sign * number

parseInstruction :: P.Parsec String () Instruction
parseInstruction = Instruction <$> parseOpCode <* P.char ' ' <*> parseNumber

parseProgram :: P.Parsec String () Program
parseProgram = do
  insts <- parseInstruction `P.endBy` P.newline
  return $ Program (V.fromList insts)

parseInput :: String -> Program
parseInput input =
  case P.parse parseProgram "" input of
    Right program -> program
    Left err -> error $ show err

findLoop :: Eq a => [a] -> (Integer, Integer)
findLoop [] = error "Empty list"
findLoop as = go as (L.tail as)
  where
    go (t : ts) (h : hs) =
      if t == h
        then findStart as hs 0
        else go ts (L.tail hs)
    findStart (t : ts) (h : hs) n =
      if t == h
        then (n, findLength t hs 1)
        else findStart ts hs (n + 1)
    findLength t (h : hs) n =
      if t == h
        then n
        else findLength t hs (n + 1)

day8 :: IO ()
day8 = do
  prg <- parseInput <$> loadInput
  let computer = mkComputer prg
      executionRun = iterate runStep computer
      (start, period) = findLoop $ cIp <$> executionRun
  print $ L.head . L.genericDrop (start + period) $ cAcc <$> executionRun
