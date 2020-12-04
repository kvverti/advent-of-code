-- Thalia Nero, Advent of Code 2020
module Puzzle4 where

import Data.List
import Data.Maybe
import Data.Traversable

type Year = Integer

data Passport = Passport { ibr :: Year
                         , iyr :: Year
                         , eyr :: Year
                         , hgt :: Integer
                         , hcl :: String
                         , ecl :: String
                         , pid :: Integer
                         , cid :: Integer }
                         
type PassportDict = [(String, String)]

-- The required keys for a passport.
keys :: [String]
keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- no cid >:)

text1 :: String
text1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
        \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
        \\n\
        \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
        \hcl:#cfa07d byr:1929\n\
        \\n\
        \hcl:#ae17e1 iyr:2013\n\
        \eyr:2024\n\
        \ecl:brn pid:760753108 byr:1931\n\
        \hgt:179cm\n\
        \\n\
        \hcl:#cfa07d eyr:2025 pid:166559648\n\
        \iyr:2011 ecl:brn hgt:59in"

-- Splits input string into passport data segments.
splitEntries :: String -> [String]
splitEntries src = unfoldr splitter $ lines src
    where splitter :: [String] -> Maybe (String, [String])
          splitter src = case break (== "") src of
                              ([], _) -> Nothing
                              (lns, src') -> Just (intercalate " " lns, dropWhile (== "") src')
                              
-- Splits input string into passport data.
passportDict :: String -> PassportDict
passportDict s = keyVal <$> unfoldr splitter s
    where splitter :: String -> Maybe (String, String)
          splitter src = case break (== ' ') src of
                              ([], _) -> Nothing
                              (entry, src') -> Just (entry, dropWhile (== ' ') src')
          keyVal :: String -> (String, String)
          keyVal entry = let (key, val') = break (== ':') entry in
                             (key, tail val')

-- Parses complete input.
input :: String -> [PassportDict]
input s = passportDict <$> splitEntries s

-- Determines whether a passport dict contains all the valid keys
validPassport :: PassportDict -> Bool
validPassport p = isJust . sequence $ flip lookup p <$> keys

-- Solves part 1 of the puzzle.
part1 :: [PassportDict] -> Int
part1 passes = length . filter id $ validPassport <$> passes

main :: IO ()
main = putStrLn =<< show . part1 . input <$> readFile "input4.txt"
