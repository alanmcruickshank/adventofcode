-- Advent of code. Day 8.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex)

main = do
    f <- readFile "day-08-input.txt"
    let i = parseInput f
    putStrLn "=== Part 1"
    print (answer1 i)

-- ----- File Processing

splitOn             :: Char -> String -> [String]
splitOn t s         = l ++ [r]
    where (l, r)        = foldl f ([], "") s
          f             :: ([String], String) -> Char -> ([String], String)
          f (a, b) c
            | c == t    = (a ++ [b], "")
            | otherwise = (a, b ++ [c])

parseInput      :: String -> [([String], [String])]
parseInput s    = map f l
    where l         = lines s
          f         :: String -> ([String], [String])
          f a       = (splitOn ' ' pfx, splitOn ' ' sfx)
            where i     = fromJust (elemIndex '|' a)
                  pfx   = take (i - 1) a
                  sfx   = drop (i + 2) a

-- ----- Answers

contains        :: Eq a => a -> [a] -> Bool  -- Only works for types which implement Eq
contains x xs   = isJust (elemIndex x xs)

answer1         :: [([String], [String])] -> Int
answer1 i       = sum(map f1 i)
    where f1        :: ([String], [String]) -> Int
          f1 (_, a) = sum(map f2 a)
            where f2        :: String -> Int
                  f2 x      = (if (contains (length x) [2, 3, 4, 7]) then 1 else 0)  -- 2,3,4,7 represents digits 1, 7, 4 & 8
