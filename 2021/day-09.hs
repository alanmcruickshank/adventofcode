-- Advent of code. Day 9.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort, transpose)
import Data.Char (digitToInt)

main = do
    f <- readFile "day-09-input.txt"
    let i = parseInput f
    putStrLn "=== Part 1"
    print (answer1 i)

-- ----- File Processing

type GridMap              = ([[Int]], (Int, Int))

parseInput      :: String -> GridMap
parseInput s    = (l', (w, h))
    where l         = lines s
          w         = length (l !! 0)
          h         = length l
          l'        = map (map digitToInt) l

-- Pad with 9 s
pad                 :: GridMap -> GridMap
pad (m, (w, h))     = (newgrid, (w + 2, h + 2))
    where newgrid   = [endrow] ++ (map padrow m) ++ [endrow]
          endrow    = (take (w + 2) (repeat 9))
          padrow r  = [9] ++ r ++ [9]

windows                     :: Int -> [a] -> [[a]]
windows w (x:xs)
    | length (x:xs) < w     = []
    | length (x:xs) == w    = [(x:xs)]
    | otherwise             = (take w (x:xs)):(windows w xs)

gridWindows                 :: Int -> GridMap -> [GridMap]
gridWindows n (m, (w, h))   = map toGrid squares
    where row_groups        = windows n m
          row_group_slices  = map (transpose.(map (windows 3))) row_groups
          squares           = foldl1 (++) row_group_slices
          toGrid s          = (s, (n, n))

-- Assume map of size 3
riskLevel                   :: GridMap -> Int
riskLevel (m, _)            = if is_low then c + 1 else 0
    where c                 = (m !! 1) !! 1
          u                 = (m !! 0) !! 1
          d                 = (m !! 2) !! 1
          l                 = (m !! 1) !! 0
          r                 = (m !! 1) !! 2
          adjs              = [u, d, l, r]
          is_low            = foldl1 (&&) (map (> c) adjs)

-- ----- File Processing

answer1                     :: GridMap -> Int
answer1 i                   = sum (map riskLevel (gridWindows 3 (pad i)))
