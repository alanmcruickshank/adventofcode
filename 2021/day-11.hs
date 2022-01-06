-- Advent of code. Day 10.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort, transpose, sortOn )
import Debug.Trace (trace)
import Data.Char (digitToInt)


main = do
    f <- readFile "day-11-example.txt"
    let i = parseFile f
    putStrLn "=== Part 1"
    print i
    print (incrGrid i)
    print (detectFlashes (incrGrid i))


type OctoGrid       = [[Int]]
type GridPoint      = (Int, Int)

parseFile           :: String -> OctoGrid
parseFile f         =  ((map (map digitToInt)).lines) f

incrGrid            :: OctoGrid -> OctoGrid
incrGrid g          = (map (map (+1))) g

detectFlashes       :: OctoGrid -> [GridPoint]
detectFlashes g     = foldl1 (++) (map genPoints g')
    where g'        = zip (map dFlashRow g) [0..]
          genPoints         :: ([Int], Int) -> [GridPoint]
          genPoints (xs, y) = zip xs (repeat y)

dFlashRow           :: [Int] -> [Int]  -- GridRow to list of row indices
dFlashRow r         = map snd f
    where r'        = zip r [0..]
          f         = filter ((>= 9).fst) r'

