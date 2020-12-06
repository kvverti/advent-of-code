-- Thalia Nero, Advent of Code 2020
module Puzzle5 where

import AdventUtil
import Data.List

text1 :: [String]
text1 = [ "BFFFBBFRRR"
        , "FFFBBBFRRR"
        , "BBFFBBFRLL" ]

-- Gets the seat ID for a seat. Literally turning this string into a number.
seatId :: String -> Integer
seatId = foldl1 ((+) . (*) 2) . map binary
    where binary :: Char -> Integer
          binary 'F' = 0
          binary 'L' = 0
          binary 'B' = 1
          binary 'R' = 1

-- Parses input.
input :: String -> [Integer]
input = map seatId . lines

-- Solves part 1.
part1 :: [Integer] -> Integer
part1 = foldl1 max

-- Solves part 2.
part2 :: [Integer] -> Integer
part2 seats = let ids = sort seats in
                  fst . head . dropWhile (uncurry (==)) $ zip [head ids..] ids

main :: IO ()
main = drive "input5.txt" input part1 part2
