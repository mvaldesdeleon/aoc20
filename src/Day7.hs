module Day7
  ( day7,
  )
where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Paths_aoc20 (getDataFileName)
import qualified Text.Parsec as P

newtype Bag = Bag String
  deriving (Show, Eq, Ord)

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
      return (count, bag)

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

isContainedInMap :: [Rule] -> M.Map Bag [Bag]
isContainedInMap = foldr processRule M.empty
  where
    processRule :: Rule -> M.Map Bag [Bag] -> M.Map Bag [Bag]
    processRule Rule {rBag = bigBag, rContents = contents} map =
      foldr (processContent bigBag) map contents
    processContent :: Bag -> (Integer, Bag) -> M.Map Bag [Bag] -> M.Map Bag [Bag]
    processContent bigBag (_, smallBag) map = M.alter (setOrInsert bigBag) smallBag map
    setOrInsert :: Bag -> Maybe [Bag] -> Maybe [Bag]
    setOrInsert bigBag curr =
      case curr of
        Just rest -> Just (bigBag : rest)
        Nothing -> Just [bigBag]

findBags :: M.Map Bag [Bag] -> Bag -> [Bag]
findBags isContainedIn bag =
  let bigBags = M.findWithDefault [] bag isContainedIn
   in bigBags ++ concat (findBags isContainedIn <$> bigBags)

containsMap :: [Rule] -> M.Map Bag [(Integer, Bag)]
containsMap = go M.empty
  where
    go map [] = map
    go map (Rule {rBag = bag, rContents = contents} : rs) = go (M.insert bag contents map) rs

countBags :: M.Map Bag [(Integer, Bag)] -> Bag -> Integer
countBags contains bag = sum $ countBagsOfType <$> contains M.! bag
  where
    countBagsOfType (count, bag) = count + (count * countBags contains bag)

day7 :: IO ()
day7 = do
  input <- parseInput <$> loadInput
  let shinyGold = Bag "shiny gold"
  print $ length . L.nub $ findBags (isContainedInMap input) shinyGold
  print $ countBags (containsMap input) shinyGold
