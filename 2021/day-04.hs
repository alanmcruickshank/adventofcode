-- Advent of code. Day 4.

import Data.List
import Data.Map (Map, lookup, insert, empty, map, foldrWithKey)
import Data.Maybe (fromJust)

main = do
    f <- readFile "day-04-input.txt"
    print "=== Part 1"
    let (call_order, cards) = processFile f
    let answer1 = winning_score call_order cards
    print answer1

-- ----- INPUT PROCESSING

-- Use a triplet of ([extracted things], prefix, unprocessed-suffix)
splitStep                           :: ([String], String, String) -> ([String], String, String)
splitStep (a, "", "")               = (a, "", "")
splitStep (a, b, "")                = (a ++ [b], "", "") -- append b to a
splitStep (a, "", '\n':'\n':xs)     = splitStep (a, "", xs)
splitStep (a, b, '\n':'\n':xs)      = splitStep (a ++ [b], "", xs)
splitStep (a, b, x:xs)              = splitStep (a, b ++ [x], xs)

-- Extract the first element of a triple
extractFirst (a, _, _)              = a

-- Unpack comma seperated string (copying previous pattern)
splitCommaSeperated                 :: String -> [Int]
splitCommaSeperated s               = extractFirst (f ([], "", s))
    where f (b, "", "")             = (b, "", "")
          f (b, c, "")              = (b ++ [read c::Int], "", "")
          f (b, "", ',':xs)         = f (b, "", xs)
          f (b, c, ',':xs)          = f (b ++ [read c::Int], "", xs)
          f (b, c, x:xs)            = f (b, c ++ [x], xs)

processFile                         :: String -> ([Int], [[Int]])
processFile s                       = (call_order, cards)
    where sections                  = splitSections s
          call_order                = splitCommaSeperated (head sections)
          cards                     = Data.List.map processRawCard (tail sections)
          processRawCard s          = (concat.(Data.List.map ((Data.List.map (\x -> read x::Int)).words)).lines) s
          splitSections s           = extractFirst (splitStep ([], "", s))

-- ----- CARD INDEXING

callOrderToIndices                  :: [Int] -> [Int] -> [Maybe Int]
callOrderToIndices call crd         = Data.List.map (\x -> elemIndex x crd) call

cardIndices                         :: [Int] -> [[Int]] -> [[Maybe Int]]
cardIndices call_order cards        = Data.List.map (\x -> callOrderToIndices call_order x) cards

removeIf                            :: (a -> Bool) -> [a] -> [a]
removeIf func []                    = []
removeIf func (h:t)                 = if func h then removeIf func t else h:(removeIf func t)

idxPairConv                         :: (Maybe Int, Int) -> (Maybe (Int, Int), Int)
idxPairConv (Nothing, a)            = (Nothing, a)
idxPairConv (Just x, a)             = (Just (indexToRc x), a)
    where indexToRc x               = (div x 5, mod x 5)  -- (row, col)

pairToList                          :: (Maybe (Int, Int), Int) -> [(String, Int)]
pairToList (Nothing, _)             = []
pairToList (Just (r, c), x)         = [("r" ++ (show r),x), ("c" ++ (show c),x)]

updateMapWithAddr                   :: Map String [Int] -> (String,  Int) -> Map String [Int]
updateMapWithAddr map (k, v)        = Data.Map.insert k nl map
    where e                         = Data.Map.lookup k map
          el                        = if e == Nothing then [] else (fromJust e)
          nl                        = v:el

addrListToMap                       :: [(String, Int)] -> Map String [Int]
addrListToMap al                    = foldl updateMapWithAddr Data.Map.empty al

reduceList                          :: [Int] -> Maybe Int
reduceList []                       = Nothing
reduceList (a:b:c:d:e:xs)           = Just(maximum (a:b:c:d:e:[]))
reduceList (a:xs)                   = Nothing

reduceMap                              :: Map String (Maybe Int) -> Maybe (String, Int)
reduceMap m                            = foldrWithKey reduceMap' Nothing m
    where reduceMap'                           :: String -> Maybe Int -> Maybe (String, Int) -> Maybe (String, Int)
          reduceMap' k Nothing Nothing         = Nothing
          reduceMap' k a Nothing               = Just (k, (fromJust a))
          reduceMap' k Nothing (Just (kb, b))  = Just (kb, b) 
          reduceMap' k a (Just (kb, b))        = if (fromJust a) < b then Just (k, (fromJust a)) else Just (k, b)

-- Higher level functions.

cardWin                             :: [Maybe Int] -> Maybe (String, Int)
cardWin card_order                  = reduceMap col_row_complete
    where card_indices              = zip card_order [0..]
          filtered_indices          = removeIf (\(x, _) -> x == Nothing) card_indices
          concatd                   = (foldl1 (++) (Data.List.map (pairToList.idxPairConv) filtered_indices))
          col_row_map               = addrListToMap concatd
          col_row_complete          = Data.Map.map reduceList col_row_map

winning_card'                       :: [Maybe (String, Int)] -> Maybe (String, Int)
winning_card' []                    = Nothing
winning_card' xs                    = foldl winning_fold Nothing xs
    where winning_fold Nothing Nothing                  = Nothing
          winning_fold Nothing a                        = a
          winning_fold a Nothing                        = a
          winning_fold (Just (ka, va)) (Just (kb, vb))  = if va < vb then Just (ka, va) else Just (kb, vb)

winning_card                        :: [[Maybe Int]] -> (Int, (String, Int))
winning_card co                     = (fromJust best_idx, fromJust best_card) -- Slightly risky to just fromJust here.
    where card_stops                = Data.List.map cardWin co
          best_card                 = winning_card' card_stops
          best_idx                  = elemIndex best_card card_stops

winning_score                       :: [Int] -> [[Int]] -> (Int, String, [Int], Int, Int, Int)
winning_score co crds               = (cd_idx, c_rc, uncalled_ns, sum_uncalled, last_called, score)
    where c_ords                    = cardIndices co crds
          (cd_idx, (c_rc, cl_idx))  = winning_card c_ords
          called_ns                 = take (cl_idx + 1) co
          uncalled_ns               = (crds !! cd_idx) \\ called_ns -- list difference
          sum_uncalled              = sum uncalled_ns
          last_called               = last called_ns
          score                     = last_called * sum_uncalled
