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
    let a2a = basinBasis i
    print (basins a2a)
    let a2b = stepBasin a2a
    print (basins a2b)
    let a2n = iterBasins a2b
    print (basins a2n)
    print (countVals (basins a2n))
    
    --print (answer2 i)

-- ----- File Processing

type GridPoint              = (Int, Int)                -- x/y 
type GridMap                = ([[Int]], (Int, Int))     -- width/height
type GridSquare             = ([[Int]], GridPoint)
type GridPointScore         = (GridSquare, Int, Int, Maybe GridPoint)   -- square, height, risk, basin


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

adjacents                   :: GridPoint -> [GridPoint]
adjacents (x, y)            = [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

stepBasin                   :: Map.Map GridPoint GridPointScore ->  Map.Map GridPoint GridPointScore
stepBasin m                 = Map.map update m
    where assigned          = Map.filter (\(_, _, _, p) -> isJust p) m
          update            :: GridPointScore -> GridPointScore
          update (g, c, r, p)
              | isJust p            = (g, c, r, p)  -- Already sorted
              | c == 9              = (g, c, r, p)  -- It's a ridge
              | length src == 0     = (g, c, r, p)  -- No already assigned neighbour - pass for now
              | otherwise           = (g, c, r, p')
              where (_, k)          = g 
                    a               = adjacents k
                    s               = Map.keys assigned
                    src             = intersect a s
                    (_, _, _, p')   = fromJust (Map.lookup (src !! 0) m)

basins                          :: Map.Map GridPoint GridPointScore -> [GridPoint]
basins m                        = foldl1 (++) (((map f).Map.toList) m)
    where f (_, (_, _, _, p))   = if isNothing p then [] else [fromJust p]

iterBasins                  :: Map.Map GridPoint GridPointScore ->  Map.Map GridPoint GridPointScore
iterBasins m
    | b == b'               = m                 -- stop iterating
    | otherwise             = iterBasins m'     -- iterate
    where m'                = stepBasin m
          b                 = basins m
          b'                = basins m'

countVals                   :: Ord a => [a] -> Map.Map a Int
countVals xs                = Map.fromListWith (+) (zip xs (repeat 1))
