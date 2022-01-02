-- Advent of code. Day 9.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort, transpose)
import Data.Char (digitToInt)
import qualified Data.Map as Map 

main = do
    f <- readFile "day-09-example.txt"
    let i = parseInput f
    putStrLn "=== Part 1"
    print (answer1 i)
    putStrLn "=== Part 2"
    print (answer2 i)

-- ----- File Processing

type GridPoint              = (Int, Int)                -- x/y 
type GridMap                = ([[Int]], (Int, Int))     -- width/height
type GridSquare             = ([[Int]], GridPoint)
type GridPointScore         = (GridSquare, Int, Int, Maybe GridPoint)


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

gridWindows                 :: GridMap -> [GridSquare]
gridWindows unpadded        = indexed_squares
    where (m, (w, h))       = pad unpadded
          row_groups        = windows 3 m
          row_group_slices  = map (transpose.(map (windows 3))) row_groups
          squares           = foldl1 (++) row_group_slices
          indexed_squares   = zip squares (map (\i -> (mod i (w - 2), div i (w - 2))) [0..])

-- Assume map of size 3
riskLevel                   :: GridSquare -> Int
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
answer1 i                   = sum (map riskLevel (gridWindows i))

answer2                     :: GridMap -> Map.Map GridPoint GridPointScore
answer2 i                   = stepBasin (basinBasis i)

basinBasis                  :: GridMap -> Map.Map GridPoint GridPointScore
basinBasis i                = m
    where windows           = gridWindows i
          m                 = Map.fromList (map f windows)
          f (m, p)          = (p, ((m, p), c, r, if r > 0 then Just p else Nothing))
            where r         = riskLevel (m, p)
                  c         = (m !! 1) !! 1

stepBasin                   :: Map.Map GridPoint GridPointScore ->  Map.Map GridPoint GridPointScore
stepBasin m                 = assigned -- m'
    where assigned          = Map.filter (\(_, _, _, p) -> isJust p) m
