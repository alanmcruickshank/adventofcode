-- Advent of code. Day 4.

import Data.List
import Data.Map (Map, lookup, insert, empty, map, foldrWithKey)
import Data.Maybe (fromJust)

main = do
    f <- readFile "day-04-example.txt"
    print "=== Part 1"
    let (call_order, cards) = process_file f
    print call_order
    print cards
    let card_orders = card_indices call_order cards
    print card_orders
    print (Data.List.map card_win card_orders)
    print (winning_card' (Data.List.map card_win card_orders))
    -- Found winning card, just need to get index and evaluate score.

-- Use a triplet of ([extracted things], prefix, unprocessed-suffix)
split_step                          :: ([String], String, String) -> ([String], String, String)
split_step (a, "", "")              = (a, "", "")
split_step (a, b, "")               = (a ++ [b], "", "") -- append b to a
split_step (a, "", '\n':'\n':xs)    = split_step (a, "", xs)
split_step (a, b, '\n':'\n':xs)     = split_step (a ++ [b], "", xs)
split_step (a, b, x:xs)             = split_step (a, b ++ [x], xs)

split_sections                      :: String -> [String]
split_sections s                    = a
    where (a, _, _)                 = split_step ([], "", s)

-- Unpack comma seperated string (copying previous pattern)
split_comma_seperated               :: String -> [Int]
split_comma_seperated s             = a
    where (a, _, _)                 = f ([], "", s)
          f (b, "", "")             = (b, "", "")
          f (b, c, "")              = (b ++ [read c::Int], "", "")
          f (b, "", ',':xs)         = f (b, "", xs)
          f (b, c, ',':xs)          = f (b ++ [read c::Int], "", xs)
          f (b, c, x:xs)            = f (b, c ++ [x], xs)

process_raw_card s                  = (concat.(Data.List.map ((Data.List.map (\x -> read x::Int)).words)).lines) s

-- Given a list of called numbers, evaluate when a card will win, and the score at that point.

-- Notes:
--  Need to turn numbers to indices.
call_order_to_indices               :: [Int] -> [Int] -> [Maybe Int]
call_order_to_indices call crd      = Data.List.map (\x -> elemIndex x crd) call

index_to_rc                         :: Int -> (Int, Int)  -- (row, col)
index_to_rc x                       = (div x 5, mod x 5)

removeif                            :: (a -> Bool) -> [a] -> [a]
removeif func []                    = []
removeif func (h:t)                 = if func h then removeif func t else h:(removeif func t)

has_null_pos                        :: (Maybe Int, Int) -> Bool
has_null_pos (Nothing, _)           = True
has_null_pos (a, _)                 = False

idx_pair_conv                       :: (Maybe Int, Int) -> (Maybe (Int, Int), Int)
idx_pair_conv (Nothing, a)          = (Nothing, a)
idx_pair_conv (Just x, a)           = (Just (index_to_rc x), a)

pair_to_list                        :: (Maybe (Int, Int), Int) -> [(String, Int)]
pair_to_list (Nothing, _)           = []
pair_to_list (Just (r, c), x)            = [("r" ++ (show r),x), ("c" ++ (show c),x)]


update_map_with_addr                :: Map String [Int] -> (String,  Int) -> Map String [Int]
update_map_with_addr map (k, v)     = Data.Map.insert k nl map
    where e                         = Data.Map.lookup k map
          el                        = if e == Nothing then [] else (fromJust e)
          nl                        = v:el

addr_list_to_map                    :: [(String, Int)] -> Map String [Int]
addr_list_to_map al                 = foldl update_map_with_addr Data.Map.empty al

reduce_list                         :: [Int] -> Maybe Int
reduce_list []                      = Nothing
reduce_list (a:b:c:d:e:xs)          = Just(maximum (a:b:c:d:e:[]))
reduce_list (a:xs)                  = Nothing

reduce_map                              :: Map String (Maybe Int) -> Maybe (String, Int)
reduce_map m                            = foldrWithKey reduce_map' Nothing m
    where reduce_map'                           :: String -> Maybe Int -> Maybe (String, Int) -> Maybe (String, Int)
          reduce_map' k Nothing Nothing         = Nothing
          reduce_map' k a Nothing               = Just (k, (fromJust a))
          reduce_map' k Nothing (Just (kb, b))  = Just (kb, b) 
          reduce_map' k a (Just (kb, b))        = if (fromJust a) < b then Just (k, (fromJust a)) else Just (k, b)

-- Higher level functions.

card_win                            :: [Maybe Int] -> Maybe (String, Int)
card_win card_order                 = reduce_map col_row_complete
    where card_indices              = zip card_order [0..]
          filtered_indices          = removeif has_null_pos card_indices
          concatd                   = (foldl1 (++) (Data.List.map (pair_to_list.idx_pair_conv) filtered_indices))
          col_row_map               = addr_list_to_map concatd
          col_row_complete          = Data.Map.map reduce_list col_row_map

process_file                        :: String -> ([Int], [[Int]])
process_file s                      = (call_order, cards)
    where sections                  = split_sections s
          call_order                = split_comma_seperated (head sections)
          cards                     = Data.List.map process_raw_card (tail sections)

card_indices                        :: [Int] -> [[Int]] -> [[Maybe Int]]
card_indices call_order cards       = Data.List.map (\x -> call_order_to_indices call_order x) cards

winning_card'                       :: [Maybe (String, Int)] -> Maybe (String, Int)
winning_card' []                    = Nothing
winning_card' xs                    = foldl winning_fold Nothing xs
    where winning_fold Nothing Nothing                  = Nothing
          winning_fold Nothing a                        = a
          winning_fold a Nothing                        = a
          winning_fold (Just (ka, va)) (Just (kb, vb))  = if va < vb then Just (ka, va) else Just (kb, vb)
