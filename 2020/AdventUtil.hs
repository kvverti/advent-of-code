-- Thalia Nero, Advent of Code 2020
module AdventUtil where

import Data.List

-- Driver function for input file IO and display.
drive :: Show b => String -> (String -> a) -> (a -> b) -> (a -> b) -> IO ()
drive file input part1 part2 = do
    putStr "Part: "
    n <- readLn
    passes <- input <$> readFile file
    putStrLn $ part n passes
    where part 1 = show . part1
          part 2 = show . part2
          part _ = const "(no part)"

-- Splits a multiline input string into blocks on blank lines.
-- The newlines in blocks are normalized to the given string.
blocks :: String -> String -> [String]
blocks sep src = unfoldr splitter $ lines src
    where splitter :: [String] -> Maybe (String, [String])
          splitter src = case break (== "") src of
                              ([], _) -> Nothing
                              (lns, src') -> Just (intercalate sep lns, dropWhile (== "") src')
