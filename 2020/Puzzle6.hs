-- Thalia Nero, Advent of Code 2020
module Puzzle6 where

import AdventUtil
import Data.List

text1 :: String
text1 = "abc\n\
        \\n\
        \a\n\
        \b\n\
        \c\n\
        \\n\
        \ab\n\
        \ac\n\
        \\n\
        \a\n\
        \a\n\
        \a\n\
        \a\n\
        \\n\
        \b"

-- Parses the input
input :: String -> [String]
input = blocks " "

-- Solves part 1.
part1 :: [String] -> Int
part1 xs = sum $ length . nub . filter (/= ' ') <$> xs

part2 :: [String] -> Int
part2 xs = sum $ length . foldl1 intersect . words <$> xs

main :: IO ()
main = drive "input6.txt" input part1 part2
