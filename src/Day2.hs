{-# LANGUAGE NamedFieldPuns #-}

module Day2
  ( day2,
  )
where

import Data.List ((!!))
import Paths_aoc20 (getDataFileName)
import Text.Parsec (Parsec, char, digit, letter, many1, newline, parse, sepEndBy, space, string)
import Prelude hiding (max, min)

data Policy = Policy
  { min :: Integer,
    max :: Integer,
    character :: Char
  }
  deriving (Show)

data Entry = Entry
  { policy :: Policy,
    password :: String
  }
  deriving (Show)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-2.txt" >>= readFile

parsePolicy :: Parsec String () Policy
parsePolicy =
  Policy <$> parseNum <* char '-' <*> parseNum <* space <*> letter
  where
    parseNum = read <$> many1 digit

parseEntry :: Parsec String () Entry
parseEntry =
  Entry <$> parsePolicy <* string ": " <*> many1 letter

parseInput :: String -> [Entry]
parseInput input =
  case parse (parseEntry `sepEndBy` newline) "" input of
    Right entries -> entries
    Left err -> error (show err)

validate :: Entry -> Bool
validate Entry {policy = Policy {min, max, character}, password} =
  let count = length . filter (== character) $ password
   in fI min <= count && count <= fI max

validate' :: Entry -> Bool
validate' Entry {policy = Policy {min, max, character}, password} =
  let first = password !! (fI min - 1)
      second = password !! (fI max - 1)
   in case (first == character, second == character) of
        (True, False) -> True
        (False, True) -> True
        _ -> False

fI :: Num a => Integer -> a
fI = fromInteger

day2 :: IO ()
day2 = do
  entries <- parseInput <$> loadInput
  print $ length $ filter (== True) $ map validate entries
  print $ length $ filter (== True) $ map validate' entries
