-- Advent of code. Day 10.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort, transpose, sortOn )
import Debug.Trace (trace)
import Data.Char (digitToInt)


main = do
    f <- readFile "day-11-input.txt"
    let i = parseFile f
    putStrLn "=== Part 1: Day 100"
    let iN = simAt 100 i
    print (snd iN)


type OctoGrid       = [[Int]]
type GridPoint      = (Int, Int)
type FlashCache     = (OctoGrid, OctoGrid) -- Grid, Flashes So Far


parseFile           :: String -> OctoGrid
parseFile f         =  ((map (map digitToInt)).lines) f

mapGrid             :: (Int -> Int) -> OctoGrid -> OctoGrid
mapGrid f g         = (map (map f)) g

incrGrid            :: OctoGrid -> OctoGrid
incrGrid g          = mapGrid (+1) g

gridOp              :: (Int -> Int -> Int) -> OctoGrid -> OctoGrid -> OctoGrid
gridOp f a b        = map (uncurry (rowOp f)) (zip a b)

rowOp               :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
rowOp f a b         = map (uncurry f) (zip a b)

width               :: OctoGrid -> Int
width g             = length (g !! 0)

height              :: OctoGrid -> Int
height g            = length g

gridSum             :: OctoGrid -> Int
gridSum g           = (sum.(map sum)) g

emptyGridLike       :: OctoGrid -> OctoGrid
emptyGridLike g     = ((take h).repeat.(take w).repeat) 0
    where w         = width g
          h         = height g

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
flashGrid g         = mapGrid (\x -> if x > 9 then 1 else 0) g

flashEffect         :: OctoGrid -> OctoGrid
flashEffect g       = effect
    where ps        = [(1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1)]
          shifts    = map (\(x, y) -> shiftGrid 0 x y g) ps
          effect    = foldl1 (gridOp (+)) shifts

flashStep           :: FlashCache -> FlashCache
flashStep (g, f)    = (gridOp (+) g new_e, gridOp (+) f new_f)
    where new_f     = gridOp (\a b -> if a == b then 0 else b) f (flashGrid g)    -- Flashes which haven't already occurred
          new_e     = flashEffect new_f

iterFlash           :: FlashCache -> FlashCache
iterFlash (g, f)
    | gridSum f == gridSum f'   = (g, f)
    | otherwise                 = iterFlash (g', f')
    where (g', f')              = flashStep (g, f)

doFlash             :: OctoGrid -> (OctoGrid, Int)
doFlash g           = (g2, gridSum f1)
    where (g1, f1)  = iterFlash (g, emptyGridLike g)
          g2        = gridOp (\a b -> if b >= 1 then 0 else a) g1 f1    -- Reset flashed octos to 0

simStep             :: (OctoGrid, Int) -> (OctoGrid, Int)
simStep (g, n)      = (g2, n + dn)
    where g1        = incrGrid g
          (g2, dn)  = doFlash g1

simAt               :: Int -> OctoGrid -> (OctoGrid, Int)
simAt d g           = (iterate simStep (g, 0)) !! d
