-- Advent of code. Day 7.

import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as Map
-- import Debug.Trace (trace) -- For Tracing

main = do
    f <- readFile "day-07-input.txt"
    let crabs = (summariseCrabs.parseOnCommas) f
    putStrLn "=== Part 1"
    --print crabs
    let estimate = estMean crabs
    print estimate
    print (search estimate crabs)

-- ----- File Processing

parseOnCommas           :: String -> [Int]
parseOnCommas s         = l ++ [read r::Int]
    where (l, r)        = foldl f ([], "") s
          f             :: ([Int], String) -> Char -> ([Int], String)
          f (a, b) ','  = (a ++ [read b::Int], "")
          f (a, b) c    = (a, b ++ [c])

coalesce                :: Maybe a -> a -> a
coalesce x d            = if isNothing x then d else fromJust x

summariseCrabs          :: [Int] -> Map.Map Int Int
summariseCrabs []       = Map.empty
summariseCrabs (x:xs)   = Map.insert x n m
    where m             = summariseCrabs xs
          n             = (coalesce (Map.lookup x m) 0) + 1

fuelToPos               :: Int -> Map.Map Int Int -> Int
fuelToPos p m           = Map.foldrWithKey f 0 m
    where f             :: Int -> Int -> Int -> Int
          f k v s       = ((abs (p - k)) * v) + s

grad                        :: Int -> Int -> Int -> Int
grad a b c
    | (b <= c) && (b <= a)  = 0 -- bottom
    | (a < b)               = (-1)
    | (c > b)               = (1)

estMean                 :: Map.Map Int Int -> Int
estMean c               = div (Map.foldrWithKey fs 0 c) (Map.foldr fv 0 c)
    where fs k v s      = s + (k * v)
          fv v s        = s + v

search                  :: Int -> Map.Map Int Int -> (Int, Int)
search start crabs      = (p, fromJust (Map.lookup p c))
    where (p, c, d)     = until (\(_,_,x) -> x == 1) f (start, Map.empty, 0)
          f             :: (Int, Map.Map Int Int, Int) -> (Int, Map.Map Int Int, Int)
          f (pn, cn, dn)
            | isNothing (Map.lookup pn cn)              = (pn, Map.insert pn (fuelToPos pn crabs) cn, dn)                   -- Cache Fail on current pos
            | isNothing (Map.lookup (pn + 1) cn)        = (pn, Map.insert (pn + 1) (fuelToPos (pn + 1) crabs) cn, dn)       -- Cache Fail on +1
            | isNothing (Map.lookup (pn - 1) cn)        = (pn, Map.insert (pn - 1) (fuelToPos (pn - 1) crabs) cn, dn)       -- Cache Fail on -1
            | g == 0                                    = (pn, cn, 1)                                                       -- Done
            | g == (-1)                                 = (pn - 1, cn, 0)                                                   -- Move Down
            | g == 1                                    = (pn + 1, cn, 0)                                                   -- Move Down
            where g         = grad pn1 pn2 pn3   -- trace (show (pn1, pn2, pn3)) (grad pn1 pn2 pn3)
                  pn1       = fromJust (Map.lookup (pn - 1) cn)
                  pn2       = fromJust (Map.lookup pn cn)
                  pn3       = fromJust (Map.lookup (pn + 1) cn)
