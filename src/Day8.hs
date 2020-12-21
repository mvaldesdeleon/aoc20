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

instance Show Instruction where
  show Instruction {iOpCode = opCode, iArg = arg} = show opCode ++ " " ++ show arg

newtype Program = Program (V.Vector Instruction)
  deriving (Show)

mkProgram :: [Instruction] -> Program
mkProgram = Program . V.fromList

data Computer = Computer
  { cProgram :: Program,
    cIp :: Integer,
    cAcc :: Integer
  }
  deriving (Show)

mkComputer :: Program -> Computer
mkComputer prg = Computer {cProgram = prg, cIp = 0, cAcc = 0}

isHalt :: Computer -> Bool
isHalt Computer {cProgram = Program insts, cIp = ip} = V.length insts == fromInteger ip

runStep :: Computer -> Computer
runStep computer@Computer {cProgram = Program insts, cIp = ip} =
  case insts V.!? fromInteger ip of
    Just inst -> runInstruction inst computer
    Nothing -> error $ "Instruction pointer out of bounds: " ++ show ip

runStepMaybe :: Computer -> Maybe Computer
runStepMaybe computer =
  if isHalt computer
    then Nothing
    else Just $ runStep computer

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = a : maybe [] (iterateMaybe f) (f a)

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
  return $ mkProgram insts

parseInput :: String -> Program
parseInput input =
  case P.parse parseProgram "" input of
    Right program -> program
    Left err -> error $ show err

findLoop :: Eq a => [a] -> Maybe (Integer, Integer)
findLoop [] = error "Empty list"
findLoop as = go as (L.tail as)
  where
    go _ [] = Nothing
    go (t : ts) (h : hs) =
      if t == h
        then Just $ findStart as hs 0
        else go ts (L.tail hs)
    findStart (t : ts) (h : hs) n =
      if t == h
        then (n, findLength t hs 1)
        else findStart ts hs (n + 1)
    findLength t (h : hs) n =
      if t == h
        then n
        else findLength t hs (n + 1)

buildVariations :: Program -> [Program]
buildVariations (Program insts) = map mkProgram $ go (V.toList insts)
  where
    go [] = [[]]
    go (inst@Instruction {iOpCode = opCode} : insts) = do
      case opCode of
        ACC -> (inst :) <$> go insts
        NOP -> ((inst :) <$> go insts) ++ [inst {iOpCode = JMP} : insts]
        JMP -> ((inst :) <$> go insts) ++ [inst {iOpCode = NOP} : insts]

findCompleteRun :: [[Computer]] -> [Computer]
findCompleteRun [] = error "No good run"
findCompleteRun (r : rs) =
  case findLoop $ cIp <$> r of
    Just _ -> findCompleteRun rs
    Nothing -> r

day8 :: IO ()
day8 = do
  prg <- parseInput <$> loadInput
  let computer = mkComputer prg
      executionRun = iterateMaybe runStepMaybe computer
      Just (start, period) = findLoop $ cIp <$> executionRun
  print $ L.head . L.genericDrop (start + period) $ cAcc <$> executionRun
  let prgs = buildVariations prg
      computers = mkComputer <$> prgs
      executionRuns = iterateMaybe runStepMaybe <$> computers
  print $ cAcc . L.last $ findCompleteRun executionRuns
