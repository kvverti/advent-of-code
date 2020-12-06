-- Thalia Nero, Advent of Code 2020
module Puzzle3 where

import AdventUtil
import Data.List

-- Forest of width, trees
data Forest = Forest Int [Foliage]
    deriving (Eq, Show)

-- A possible foliage state.
data Foliage = Full | Clear
    deriving (Eq, Show)

-- Returns how significant the foliage represented.
density :: Foliage -> Integer
density Full = 1
density Clear = 0

text1 :: String
text1 = "..##.......\n\
        \#...#...#..\n\
        \.#....#..#.\n\
        \..#.#...#.#\n\
        \.#...##..#.\n\
        \..#.##.....\n\
        \.#.#.#....#\n\
        \.#........#\n\
        \#.##...#...\n\
        \#...##....#\n\
        \.#..#...#.#"

-- Converts an input string grid to a Forest.
input :: String -> Forest
input s = let trees = map disc <$> lines s in
              Forest (length $ head trees) (concat trees)
    where disc :: Char -> Foliage
          disc '#' = Full
          disc '.' = Clear

-- Evaluates a Forest path using the given (fall, run).
nextPoint :: (Int, Int) -> (Int, Forest) -> (Int, Forest)
nextPoint (fall, run) (x, Forest w ts) = let fall' = fall * w
                                             run' = (x + run) `mod` w
                                             ts' = drop fall' ts in
                                             (run', Forest w ts')

-- Given the step, compute the foliage encountered along the path through the forest.
path :: (Int, Int) -> Forest -> [Foliage]
path step forest = let next = nextPoint step in
                       map toTree . takeWhile unfinished $ iterate' next (0, forest)
    where toTree :: (Int, Forest) -> Foliage
          toTree (x, Forest _ ts) = ts !! x
          unfinished :: (Int, Forest) -> Bool
          unfinished (x, Forest _ ts) = drop x ts /= []

-- Computes how many trees are hit with the given step.
treesHit :: (Int, Int) -> Forest -> Integer
treesHit step forest = sum $ density <$> path step forest

-- Solves part 1 of the puzzle.
part1 :: Forest -> Integer
part1 = treesHit (1, 3)

-- Solves part 2 of the puzzle.
part2 :: Forest -> Integer
part2 forest = product $ flip treesHit forest <$> slopes
    where slopes :: [(Int, Int)]
          slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]

main :: IO ()
main = drive "input3.txt" input part1 part2
