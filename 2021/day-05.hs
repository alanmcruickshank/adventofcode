-- Advent of code. Day 4.

import Data.List
import Data.Map (Map, lookup, insert, empty, map, foldrWithKey)
import Data.Maybe (fromJust)

main = do
    f <- readFile "day-05-input.txt"
    let l = extractLines f
    putStrLn "=== Part 1"
    print (answer1 l)

-- ----- Data Types

type IntPoint                = (Int, Int)
type IntLine                 = (IntPoint, IntPoint)


-- ----- File Processing

extractLines                :: String -> [IntLine]
extractLines f              = Data.List.map processLine (lines f)

processLine                 :: String -> IntLine
processLine s               = (processPair first_pair, processPair second_pair)
    where idx               = fromJust (elemIndex '-' s)
          first_pair        = take (idx - 1) s
          second_pair       = drop (idx + 3) s

processPair                 :: String -> IntPoint
processPair s               = (read first::Int, read second::Int)
    where idx               = fromJust (elemIndex ',' s)
          first             = take idx s
          second            = drop (idx + 1) s

-- ----- Line Functions

isHorizontal                        :: IntLine -> Bool
isHorizontal ((_, y1), (_, y2))     = y1 == y2

isVertical                          :: IntLine -> Bool
isVertical ((x1, _), (x2, _))       = x1 == x2

isOrtho x                           = (isHorizontal x) || (isVertical x)

lineToPoints                        :: IntLine -> [IntPoint]
lineToPoints ((x1, y1), (x2, y2))
    | isHorizontal ((x1, y1), (x2, y2))     = Data.List.map (\x -> (x, y1)) (pointRange x1 x2)
    | isVertical ((x1, y1), (x2, y2))       = Data.List.map (\y -> (x1, y)) (pointRange y1 y2)
    | otherwise                             = []

pointRange                          :: Int -> Int -> [Int]
pointRange a b
    | a == b                        = [a]
    | a > b                         = reverse [b..a]
    | b > a                         = [a..b]

pointListFold                       :: [IntPoint] -> Map IntPoint Int
pointListFold pl                    = foldl pointListFold' Data.Map.empty pl
    where pointListFold'            :: Map IntPoint Int -> IntPoint -> Map IntPoint Int
          pointListFold' m p        = Data.Map.insert p n2 m
            where n1                = Data.Map.lookup p m
                  n2                = if n1 == Nothing then 1 else 1 + (fromJust n1)


-- ----- Answers

answer1                             :: [IntLine] -> Int
answer1 l                           = Data.Map.foldrWithKey (\p c n -> if c >= 2 then n + 1 else n) 0 (answer1' l)
    where answer1'                  :: [IntLine] -> Map IntPoint Int
          answer1' l                = pointListFold  (foldl (++) [] (Data.List.map lineToPoints l))
