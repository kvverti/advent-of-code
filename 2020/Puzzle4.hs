-- Thalia Nero, Advent of Code 2020
module Puzzle4 where

import Data.Ix
import Data.List
import Data.Maybe

type Year = Integer

-- A passport.
data Passport = Passport { byr :: Year
                         , iyr :: Year
                         , eyr :: Year
                         , hgt :: String
                         , hcl :: String
                         , ecl :: String
                         , pid :: String }
    deriving (Eq, Show)

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

invalid1 :: String
invalid1 = "eyr:1972 cid:100\n\
           \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
           \\n\
           \iyr:2019\n\
           \hcl:#602927 eyr:1967 hgt:170cm\n\
           \ecl:grn pid:012533040 byr:1946\n\
           \\n\
           \hcl:dab227 iyr:2012\n\
           \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
           \\n\
           \hgt:59cm ecl:zzz\n\
           \eyr:2038 hcl:74454a iyr:2023\n\
           \pid:3556412378 byr:2007"

valid1 :: String
valid1 = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
         \hcl:#623a2f\n\
         \\n\
         \eyr:2029 ecl:blu cid:129 byr:1989\n\
         \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
         \\n\
         \hcl:#888785\n\
         \hgt:164cm byr:2001 iyr:2015 cid:88\n\
         \pid:545766238 ecl:hzl\n\
         \eyr:2022\n\
         \\n\
         \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

-- Determines whether a passport is "strictly valid"
strictlyValid :: Passport -> Bool
strictlyValid (Passport byr iyr eyr hgt hcl ecl pid) =
    inRange (1920, 2002) byr &&
    inRange (2010, 2020) iyr &&
    inRange (2020, 2030) eyr &&
    height hgt &&
    hair hcl &&
    eye ecl &&
    passId pid
    where height :: String -> Bool
          height hgt = let (v', unit) = span (inRange ('0', '9')) hgt
                           v = read v' in
                           case unit of
                                "cm" -> inRange (150, 193) v
                                "in" -> inRange (59, 76) v
                                _ -> False
          hair :: String -> Bool
          hair hcl = let (prefix, col) = splitAt 1 hcl in
                         prefix == "#" &&
                         length col == 6 &&
                         all (\c -> inRange ('0', '9') c || inRange ('a', 'f') c) col
          eye :: String -> Bool
          eye ecl = elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          passId :: String -> Bool
          passId pid = length pid == 9 && all (inRange ('0', '9')) pid

type PassportDict = [(String, String)]

-- The required keys for a passport.
keys :: [String]
keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- no cid >:)

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

passport :: PassportDict -> Maybe Passport
passport pd = do
    byr <- read <$> lookup "byr" pd
    iyr <- read <$> lookup "iyr" pd
    eyr <- read <$> lookup "eyr" pd
    hgt <- lookup "hgt" pd
    hcl <- lookup "hcl" pd
    ecl <- lookup "ecl" pd
    pid <- lookup "pid" pd
    return $ Passport byr iyr eyr hgt hcl ecl pid

-- Solves part 1 of the puzzle.
part1 :: [PassportDict] -> Int
part1 passes = length . catMaybes $ passport <$> passes

-- Solves part 2 of the puzzle.
part2 :: [PassportDict] -> Int
part2 passes = length . filter strictlyValid . catMaybes $ passport <$> passes

main :: IO ()
main = do
    putStr "Part: "
    n <- readLn
    passes <- input <$> readFile "input4.txt"
    putStrLn . show $ part n passes
    where part 1 = part1
          part 2 = part2
          part _ = const 0

