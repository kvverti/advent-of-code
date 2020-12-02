-- Thalia Nero, Advent of Code 2020
module Puzzle1 where

-- Test input
test1 :: [Integer]
test1 = [1721, 979, 366, 299, 675, 1456]

-- Evaluates the n-way cartesian product of xs.
cartesian :: Int -> [Integer] -> [[Integer]]
cartesian 0 _ = [[]]
cartesian n xs = (:) <$> xs <*> cartesian (n - 1) xs

-- Returns whether the given list sums to the given value.
sumTo :: Integer -> [Integer] -> Bool
sumTo target = (== target) . foldl (+) 0

-- Given a data size and a list, return the product of the sub-sequences
-- of length n that sum to 2020.
products :: Int -> [Integer] -> [Integer]
products n = map (foldl (*) 1) . filter (sumTo 2020) . cartesian n

-- Parses a newline separated list of numbers into a list.
input :: String -> [Integer]
input = map read . lines

main :: IO ()
main = do
    putStr "Number of entries to sum: "
    n <- readLn
    xs <- input <$> readFile "input1.txt"
    putStrLn . show . head $ products n xs
