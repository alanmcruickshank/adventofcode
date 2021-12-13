-- Advent of code. Day 3.

main = do
    f <- readFile "day-03-input.txt"
    -- Get the counts
    let c = (lines_to_counts.lines) f
    print c
    -- Convert to factor
    let r = counts_to_factor c
    print r

-- All values are 0 or 1, so keeping count can be done in a pair.
-- A list of values becomes a list of pairs.

-- Convert single char to a pair
convert_char        :: Char -> (Integer, Integer)
convert_char '0'    = (1, 0)
convert_char '1'    = (0, 1)

-- Convert pairs back to integers
convert_pair        :: (Integer, Integer) -> Integer
convert_pair (a, b) = if a > b then 0 else 1

-- Convert list of bits to integer.
-- NB: Using a lambda expression for folded bitshifting
bitlist_to_int          :: [Integer] -> Integer
bitlist_to_int l        = foldl1 (\x y -> (x * 2) + y) l

bitlist_complement      :: [Integer] -> [Integer]
bitlist_complement l    = map (\x -> 1 - x) l

-- Convert a string to a list of pairs
convert_string      :: String -> [(Integer, Integer)]
convert_string s    = map convert_char s

-- Add two pairs
sum_pair                :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sum_pair (a, b) (c, d)  = (a + c, b + d)

-- Add two pair strings.
-- The uncurry takes sum_pair (which expects two inputs), to something which affects one as a pair as returned from zip.
sum_lists               :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
sum_lists a b           = map (uncurry sum_pair) (zip a b)

-- Go straight from lines to counts
lines_to_counts l       = foldl1 sum_lists (map convert_string l)

-- Go staight from counts to combined factor
counts_to_bitlist c     = map convert_pair c
bitlist_to_factor b     = (bitlist_to_int b) * ((bitlist_to_int.bitlist_complement) b)
counts_to_factor        = bitlist_to_factor.counts_to_bitlist
