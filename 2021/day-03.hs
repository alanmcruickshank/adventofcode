-- Advent of code. Day 3.

main = do
    f <- readFile "day-03-input.txt"
    let l = lines f
    print "=== Part 1"
    print (lines_to_factors1 l)
    print "=== Part 2"
    print (lines_to_factors2 l)

-- All values are 0 or 1, so keeping count can be done in a pair.
-- A list of values becomes a list of pairs.

-- ------------ PART 1 Functions

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
counts_to_basic_factor  = bitlist_to_factor.counts_to_bitlist

lines_to_factors1       = counts_to_basic_factor.lines_to_counts

-- ------------ PART 1 Functions

get_pair_at_index_from_lines        :: ([String], Int) -> (Integer, Integer)
get_pair_at_index_from_lines (l, i) = (lines_to_counts l) !! i

-- True means we're looking for most common value or 1 if equal.
-- False means we're looking for the least common value or 0 if equal.
desired_char_from_pair              :: (Integer, Integer) -> Bool -> Char
desired_char_from_pair (a, b) True  = if a <= b then '1' else '0'
desired_char_from_pair (a, b) False = if a > b then '1' else '0'

desired_char_at_index_from_lines       :: ([String], Int) -> Bool -> Char
desired_char_at_index_from_lines a b   = desired_char_from_pair (get_pair_at_index_from_lines a) b

filter_lines_a                :: ([String], Int) -> Bool -> ([String], Int)
filter_lines_a (l, i) b       = (filter (\x -> x !! i == (desired_char_at_index_from_lines (l, i) b)) l, i + 1)

filter_lines_b                :: ([String], Int) -> Bool -> String
filter_lines_b ((x:[]), _) _  = x          -- Single value left
filter_lines_b ((x:xs), i) b  = filter_lines_b (filter_lines_a ((x:xs), i) b) b -- More than one value left

filter_lines                  :: [String] -> Bool -> String
filter_lines l b              = filter_lines_b (l, 0) b

string_to_bitlist             :: [Char] -> [Integer]
string_to_bitlist l           = map (\x -> if x == '0' then 0 else 1) l

binstring_to_int              :: String -> Integer
binstring_to_int              = bitlist_to_int.string_to_bitlist

lines_to_factors2 l           = (binstring_to_int(filter_lines l True)) * (binstring_to_int(filter_lines l False))
