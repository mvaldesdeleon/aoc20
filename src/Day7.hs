module Day7
  ( day7,
  )
where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Paths_aoc20 (getDataFileName)
import qualified Text.Parsec as P

newtype Bag = Bag String
  deriving (Show, Eq)

data Rule = Rule
  { rBag :: Bag,
    rContents :: [(Integer, Bag)]
  }
  deriving (Show)

loadInput :: IO String
loadInput = getDataFileName "inputs/day-7.txt" >>= readFile

parseBag :: P.Parsec String () Bag
parseBag = do
  word1 <- parseWord
  P.char ' '
  word2 <- parseWord
  return $ Bag $ word1 ++ " " ++ word2
  where
    parseWord = P.many1 (P.noneOf [' '])

parseContents :: P.Parsec String () [(Integer, Bag)]
parseContents = P.try (P.string "no other bags" $> []) <|> (parseContent `P.sepBy1` P.string ", ")
  where
    parseContent = do
      count <- (\d -> read [d]) <$> P.digit
      P.char ' '
      bag <- parseBag
      P.string " bag"
      P.optional $ P.char 's'
      return $ (count, bag)

parseRule :: P.Parsec String () Rule
parseRule = do
  bag <- parseBag
  P.string " bags contain "
  contents <- parseContents
  P.char '.'
  return $ Rule {rBag = bag, rContents = contents}

parseInput :: String -> [Rule]
parseInput input =
  case P.parse (parseRule `P.endBy` P.newline) "" input of
    Right rules -> rules
    Left err -> error $ show err

day7 :: IO ()
day7 = do
  input <- parseInput <$> loadInput
  print $ input
