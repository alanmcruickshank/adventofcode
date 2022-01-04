-- Advent of code. Day 10.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort, transpose, sortOn )
import Debug.Trace (trace)


main = do
    f <- readFile "day-10-input.txt"
    let i = lines f
    putStrLn "=== Part 1"
    print (answer1 i)


openers             = ['(', '[', '{', '<']
closers             = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
scores              = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

-- Return codes
-- 0 = success
-- 1 = incomplete
-- 2 = corrupt

parse               :: String -> (Int, Int) -- retcode, score
parse s
    | success       = (0, 0)
    | str == ""     = (1, 0)
    | otherwise     = (2, score)
    where (stk, str, success, score)    = parse' "" s

parse'              :: String -> String -> (String, String, Bool, Int)          -- closer-stack, string, success, score
parse' stack str
    | (str == []) && (stack == [])              = ([], [], True, 0)             -- Happy Ending
    | (str == []) && (stack /= [])              = (stack, [], False, 0)         -- Finished without closing (incomplete)
    -- | is_opnr                                   = trace (show ((closer:stack), rn)) (parse' (closer:stack) rn)
    | is_opnr                                   = parse' (closer:stack) rn      -- Found another opener (add expected closer to stack)
    | (stack == []) || (r /= k)                 = (stack, str, False, score)    -- No stack to match closer, or found WRONG closer
    -- | (r == k)                                  = trace (show (kn, rn)) (parse' kn rn)
    | (r == k)                                  = parse' kn rn                  -- Found correct closer
    
    where r:rn      = str
          k:kn      = stack
          is_opnr   = elem r openers        -- is it an opener
          closer    = fromJust (lookup r closers)
          score     = fromJust (lookup r scores)

answer1     :: [String] -> Int
answer1     = sum.(map (snd.parse))
