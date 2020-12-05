-- Thalia Nero, Advent of Code 2020
module Puzzle5 where

text1 :: [String]
text1 = [ "BFFFBBFRRR"
        , "FFFBBBFRRR"
        , "BBFFBBFRLL" ]

zeros :: [Char]
zeros = "FL"

ones :: [Char]
ones = "BR"

-- Gets the seat ID for a seat. Literally turning this string into a number.
seatId :: String -> Integer
seatId = foldl1 ((+) . (*) 2) . map binary
    where binary :: Char -> Integer
          binary 'F' = 0
          binary 'L' = 0
          binary 'B' = 1
          binary 'R' = 1

-- Solves part 1.
part1 :: [String] -> Integer
part1 = foldl1 max . map seatId

main :: IO ()
main = putStrLn =<< show . part1 . lines <$> readFile "input5.txt"
