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
    print (flashGrid (incrGrid i))
    print (shiftGrid 0 2 2 (flashGrid (incrGrid i)))


type OctoGrid       = [[Int]]
type GridPoint      = (Int, Int)

parseFile           :: String -> OctoGrid
parseFile f         =  ((map (map digitToInt)).lines) f

mapGrid             :: (Int -> Int) -> OctoGrid -> OctoGrid
mapGrid f g         = (map (map f)) g

incrGrid            :: OctoGrid -> OctoGrid
incrGrid g          = mapGrid (+1) g

width               :: OctoGrid -> Int
width g             = length (g !! 0)

height              :: OctoGrid -> Int
height g            = length g

shiftGrid           :: Int -> Int -> Int -> OctoGrid -> OctoGrid
shiftGrid f x y g   = r_pfx ++ g'2 ++ r_sfx
    where g'1       = map (\r -> c_pfx ++ (take ((width g) - x) (drop (-x) r)) ++ c_sfx) g
          g'2       = take ((height g) - y) (drop (-y) g'1)                 -- trim rows
          c_pfx     = if x < 1 then [] else take x $ repeat f
          c_sfx     = if x > -1 then [] else take (-x) $ repeat f
          r_pfx     = if y < 1 then [] else take y $ repeat blank_r         -- flex my $
          r_sfx     = if y > -1 then [] else take (-y) $ repeat blank_r     -- flex my $
          blank_r   = take (width g) (repeat f)

flashGrid           :: OctoGrid -> OctoGrid -- (1s where there is a flash, 0 otherwise)
flashGrid g         = mapGrid (\x -> if x >= 9 then 1 else 0) g
