-- Advent of code. Day 10.

import Data.Maybe (fromJust, isNothing, isJust)
import Data.List (elemIndex, find, (\\), intersect, sort, transpose, sortOn )
import Debug.Trace (trace)


main = do
    f <- readFile "day-10-input.txt"
    let i = lines f
    putStrLn "=== Part 1"
    print (answer1 i)
    putStrLn "=== Part 2"
    print (answer2 i)


openers             = ['(', '[', '{', '<']
closers             = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
scores              = [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
scores2             = [(')', 1), (']', 2), ('}', 3), ('>', 4)]

-- Return codes
-- 0 = success
-- 1 = incomplete
-- 2 = corrupt

parse               :: String -> (Int, Int) -- retcode, score
parse s
    | success       = (0, 0)
    -- | str == ""     = trace (show (reverse stk, completion_score)) (1, completion_score)
    | str == ""     = (1, completion_score)
    | otherwise     = (2, score)
    where (stk, str, success, score)    = parse' "" s
          completion_score              = completionScore (reverse stk)
  
completionScore         :: String -> Int
completionScore []      = 0
completionScore (s:sn)  = (fromJust (lookup s scores2)) + (5 * (completionScore sn))     

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

midVal          :: Ord a => [a] -> a
midVal l        = l' !! idx
    where l'    = sort l
          ln    = length l
          idx   = div (ln - (mod ln 2)) 2

answer1     :: [String] -> Int
answer1     = sum.(map snd).(filter ((== 2).fst)).(map parse)

answer2     :: [String] -> Int
answer2     = midVal.(map snd).(filter ((== 1).fst)).(map parse)
