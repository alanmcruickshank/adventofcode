-- Advent of code. Day 8.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort)
import Data.Char (chr)

main = do
    f <- readFile "day-08-input.txt"
    let i = parseInput f
    putStrLn "=== Part 1"
    print (answer1 i)
    putStrLn "=== Part 2"
    print (answer2 i)

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

-- ----- Utils

contains        :: Eq a => a -> [a] -> Bool  -- Only works for types which implement Eq
contains x xs   = isJust (elemIndex x xs)

hasLen          :: Int -> [a] -> Bool
hasLen l x      = (length x) == l

-- ----- Digit Processing

digitOptions    :: String -> [Int]
digitOptions s
    | l == 2    = [1]
    | l == 3    = [7]
    | l == 4    = [4]
    | l == 5    = [2, 3, 5]
    | l == 6    = [0, 6, 9]
    | l == 7    = [8]
    | otherwise = []
    where l     = length s

-- deduce the A segment (the easiest)
deduceA         :: [String] -> Char
deduceA l       = (c7 \\ c1) !! 0
    where c1    = fromJust (find (hasLen 2) l)
          c7    = fromJust (find (hasLen 3) l)

-- deduce the F segment (by deduction)
deduceF         :: [String] -> Char
deduceF l       = c !! 0
    where c1    = fromJust (find (hasLen 2) l)
          s6    = filter (hasLen 6) l
          i     = map (\x -> intersect x c1) s6 -- intersect each with 1
          c     = foldl intersect c1 i          -- intersect fold

-- deduce the C segment (by elimination using F)
deduceC         :: [String] -> Char
deduceC l       = c !! 0
    where c1    = fromJust (find (hasLen 2) l)
          c     = c1 \\ [deduceF l]
    
-- deduce the horizontals  (using the 3 character)
deduceHoriz     :: [String] -> String
deduceHoriz l   = fromJust (find (hasLen 3) i)  -- only the horizontal parts of 3 
    where c1    = fromJust (find (hasLen 2) l)
          s6    = filter (hasLen 5) l
          i     = map (\x -> (\\) x c1) s6      -- diff each with 1

-- deduce the B segment (similar to F)
deduceB         :: [String] -> Char
deduceB l       = c !! 0
    where c1    = fromJust (find (hasLen 2) l)
          s6    = filter (hasLen 6) l
          i     = foldl intersect "abcdefg" s6  -- intersect each
          c     = i \\ (c1 ++ (deduceHoriz l))

-- deduce the E segment (by deduction)
deduceE         :: [String] -> Char
deduceE l       = c !! 0
    where c1    = fromJust (find (hasLen 2) l)
          c     = "abcdefg" \\ ((deduceHoriz l) ++ c1 ++ [deduceB l])

-- deduce the D segment (by deduction)
deduceD         :: [String] -> Char
deduceD l       = c !! 0
    where c4    = fromJust (find (hasLen 4) l)
          c1    = fromJust (find (hasLen 2) l)
          c     = c4 \\ (c1 ++ [deduceB l])

-- deduce the G segment by elimination
deduceG         :: [String] -> Char
deduceG l       = c !! 0
    where c     = (deduceHoriz l)  \\ [deduceA l, deduceD l]

deduceCipher    :: [String] -> String
deduceCipher l  = [deduceA l, deduceB l, deduceC l, deduceD l, deduceE l, deduceF l, deduceG l]

translate           :: String -> String -> String
translate c []      = []
translate c (x:xs)  = (chr (fromJust (elemIndex x c) + 97)):(translate c xs)

toCharDigit         :: String -> Char
toCharDigit s'
    | s == "cf"     = '1'
    | s == "acf"    = '7' 
    | s == "bcdf"   = '4'
    | s == "acdeg"  = '2'
    | s == "acdfg"  = '3'
    | s == "abdfg"  = '5'
    | s == "abcefg" = '0'
    | s == "abdefg" = '6'
    | s == "abcdfg" = '9'
    | s == "abcdefg"= '8'
    where s         = sort s'

decode              :: String -> [String] -> Int
decode c a          = read (map (toCharDigit.(translate c)) a)::Int

-- ----- Answers

answer1         :: [([String], [String])] -> Int
answer1 i       = sum(map f1 i)
    where f1        :: ([String], [String]) -> Int
          f1 (_, a) = sum(map f2 a)
            where f2        :: String -> Int
                  f2 x      = (if (contains (length x) [2, 3, 4, 7]) then 1 else 0)  -- 2,3,4,7 represents digits 1, 7, 4 & 8

answer2         :: [([String], [String])] -> Int
answer2 i       = sum(map f i)
    where f         :: ([String], [String]) -> Int
          f (a, b)  = decode (deduceCipher a) b
