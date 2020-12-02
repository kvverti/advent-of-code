-- Thalia Nero, Advent of Code 2020
module Puzzle2 where

import Data.Maybe
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

-- Determines whether a password fulfills the given PassPolicy, part 1.
valid1 :: (PassPolicy, String) -> Bool
valid1 (PassPolicy least most ch, pass) =
    let ntimes = length $ filter (== ch) pass in
        ntimes >= least && ntimes <= most

-- Determines whether a password fulfills the given PassPolicy, part 2.
valid2 :: (PassPolicy, String) -> Bool
valid2 (PassPolicy pos1 pos2 ch, pass) =
    let passLs = zip [1..] pass
        charEq pos = fromMaybe False $ (== ch) <$> lookup pos passLs in
        charEq pos1 /= charEq pos2

-- Parses an input string into structured data.
input :: String -> [(PassPolicy, String)]
input s = policy <$> words . (\\ ":") <$> lines s
    where policy :: [String] -> (PassPolicy, String)
          policy [bs, [ch], ps] = let (least, most') = break (== '-') bs in
                                      (PassPolicy (read least) (read . tail $ most') ch, ps)

-- Switch on which part.
part :: Int -> (PassPolicy, String) -> Bool
part 1 = valid1
part 2 = valid2
part _ = const False

-- Compute the solution based on the part.
solution :: Int -> [(PassPolicy, String)] -> Int
solution which = length . filter id . map (part which)

main :: IO ()
main = do
    putStr "Part: "
    part <- readLn
    xs <- input <$> readFile "input2.txt"
    putStrLn $ show . solution part $ xs

