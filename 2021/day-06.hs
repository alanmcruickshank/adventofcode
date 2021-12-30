-- Advent of code. Day 6.

import Data.List
import Data.Maybe (fromJust)
import Data.Char (digitToInt)

main = do
    f <- readFile "day-06-input.txt"
    let fish = extractFish f
    putStrLn "=== Part 1"
    putStrLn ("Day 18: " ++ (show (fishAt fish 18)))
    putStrLn ("Day 80: " ++ (show (fishAt fish 80)))


-- Count fish at each lifestage
extractFish                 :: String -> [Int]
extractFish s               = foldl f [0,0,0,0,0,0,0,0,0] s'
    where s'                = divideString s
          f                 :: [Int] -> Int -> [Int]
          f l i             = (take i l) ++ [(l !! i) + 1] ++ (drop (i + 1) l)

divideString                :: String -> [Int]
divideString s              = foldl f [] s
    where f                 :: [Int] -> Char -> [Int]
          f xs ','          = xs
          f xs d            = (digitToInt d):xs

dayStep                     :: [Int] -> [Int]
dayStep l                   = prefix ++ suffix
    where prefix            = (take 6 (drop 1 l))
          spawners          = l !! 0
          suffix            = ((l !! 7) + spawners):(l !! 8):(spawners):[]

fishAt                      :: [Int] -> Int -> Int
fishAt f n                  = sum ((iterate dayStep f) !! n)
