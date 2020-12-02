-- Thalia Nero, Advent of Code 2020
module Puzzle2 where

import Data.List

data PassPolicy = PassPolicy Int Int Char
    deriving (Eq, Show)

test1 :: [(PassPolicy, String)]
test1 = [ (PassPolicy 1 3 'a', "abcde")
        , (PassPolicy 1 3 'b', "cdefg")
        , (PassPolicy 2 9 'c', "ccccccccc") ]
        
test1Text :: String
test1Text = "1-3 a: abcde\n\
            \1-3 b: cdefg\n\
            \2-9 c: ccccccccc"

-- Determines whether a password fulfills the given PassPolicy.
valid :: PassPolicy -> String -> Bool
valid (PassPolicy least most ch) pass =
    let ntimes = length $ filter (== ch) pass in
        ntimes >= least && ntimes <= most

-- Parses an input string into structured data.
input :: String -> [(PassPolicy, String)]
input s = policy <$> words . (\\ ":") <$> lines s
    where policy :: [String] -> (PassPolicy, String)
          policy [bs, [ch], ps] = let (least, most') = break (== '-') bs in
                                    (PassPolicy (read least) (read . tail $ most') ch, ps)
                                    
part1 :: [(PassPolicy, String)] -> Int
part1 ps = length $ filter id $ map (uncurry valid) ps

main :: IO ()
main = putStrLn =<< show . part1 . input <$> readFile "input2.txt"

