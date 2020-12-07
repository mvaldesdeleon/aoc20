module Day4
  ( day4,
  )
where

import Control.Monad (replicateM_)
import Paths_aoc20 (getDataFileName)
import Text.Parsec (Parsec, anyChar, char, endBy1, many1, manyTill, newline, noneOf, parse, sepBy1, sepEndBy1, space, string, try, (<?>), (<|>))

loadInput :: IO String
loadInput = getDataFileName "inputs/day-4.txt" >>= readFile

data Field = BYR | IYR | EYR | HGT | HCL | ECL | PID | CID deriving (Show)

data Value = Value Field String deriving (Show)

data Document = Document [Value] deriving (Show)

parseField :: Parsec String () Field
parseField =
  pBYR
    <|> pIYR
    <|> pEYR
    <|> pHGT
    <|> pHCL
    <|> pECL
    <|> pPID
    <|> pCID
    <?> "field"
  where
    pBYR = try (BYR <$ string "byr")
    pIYR = try (IYR <$ string "iyr")
    pEYR = try (EYR <$ string "eyr")
    pHGT = try (HGT <$ string "hgt")
    pHCL = try (HCL <$ string "hcl")
    pECL = try (ECL <$ string "ecl")
    pPID = try (PID <$ string "pid")
    pCID = try (CID <$ string "cid")

parseValue :: Parsec String () Value
parseValue =
  Value <$> parseField <* char ':' <*> many1 (noneOf [' ', '\n'])

parseDocument :: Parsec String () Document
parseDocument =
  Document <$> (parseValue `sepEndBy1` (char ' ' <|> char '\n'))

parseInput :: String -> [Document]
parseInput input = case parse (parseDocument `sepBy1` char '\n') "" input of
  Right docs -> docs
  Left err -> error $ show err

checksum :: Document -> Integer
checksum (Document values) = sum . map fieldChecksum $ values
  where
    fieldChecksum (Value field _) = case field of
      BYR -> 1
      IYR -> 2
      EYR -> 4
      HGT -> 8
      HCL -> 16
      ECL -> 32
      PID -> 64
      CID -> 128

isValid :: Document -> Bool
isValid doc = checksum doc == 127 || checksum doc == 255

isValid' :: Document -> Bool
isValid' doc@(Document values) = (checksum doc == 127 || checksum doc == 255) && all validValues values
  where
    validValues (Value field value) =
      case field of
        -- byr (Birth Year) - four digits; at least 1920 and at most 2002.
        BYR -> length value == 4 && "1920" <= value && value <= "2002"
        -- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
        IYR -> length value == 4 && "2010" <= value && value <= "2020"
        -- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        EYR -> length value == 4 && "2020" <= value && value <= "2030"
        HGT -> validHeight value
        HCL -> validColor value
        -- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        ECL -> value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        -- pid (Passport ID) - a nine-digit number, including leading zeroes
        PID -> length value == 9 && "000000000" <= value && value <= "999999999"
        CID -> True

-- hgt (Height) - a number followed by either cm or in:
validHeight :: String -> Bool
-- If cm, the number must be at least 150 and at most 193.
validHeight value@(_ : _ : _ : 'c' : 'm' : []) =
  let cms = take 3 value
   in "150" <= cms && cms <= "193"
-- If in, the number must be at least 59 and at most 76.
validHeight value@(_ : _ : 'i' : 'n' : []) =
  let ins = take 2 value
   in "59" <= ins && ins <= "76"
validHeight _ = False

validColor :: String -> Bool
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validColor ('#' : value) = length value == 6 && all (`elem` (['0' .. '9'] ++ ['a' .. 'f'])) value
validColor _ = False

day4 :: IO ()
day4 = do
  documents <- parseInput <$> loadInput
  print $ sum [1 | doc <- documents, isValid doc]
  print $ sum [1 | doc <- documents, isValid' doc]
