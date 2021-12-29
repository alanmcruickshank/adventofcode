-- Advent of code. Day 4.

import Data.List
import Data.Map (Map, lookup, insert, empty, map, foldrWithKey)
import Data.Maybe (fromJust)

main = do
    f <- readFile "day-04-input.txt"
    let (call_order, cards) = processFile f
    print "=== Part 1"
    print (answer1 call_order cards)
    print "=== Part 2"
    print (answer2 call_order cards)


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


-- ----- Higher level functions.

cardWin                             :: [Maybe Int] -> Maybe (String, Int)
cardWin card_order                  = reduceMap col_row_complete
    where card_indices              = zip card_order [0..]
          filtered_indices          = removeIf (\(x, _) -> x == Nothing) card_indices
          concatd                   = (foldl1 (++) (Data.List.map (pairToList.idxPairConv) filtered_indices))
          col_row_map               = addrListToMap concatd
          col_row_complete          = Data.Map.map reduceList col_row_map

-- Generic find best or worst card
cardFold                                    :: (Int -> Int -> Bool) -> Maybe (String, Int) -> Maybe (String, Int) -> Maybe (String, Int)
cardFold f Nothing Nothing                  = Nothing
cardFold f Nothing a                        = a
cardFold f a Nothing                        = a
cardFold f (Just (ka, va)) (Just (kb, vb))  = if f va vb then Just (ka, va) else Just (kb, vb)

chooseCard                          :: (Int -> Int -> Bool) -> [[Maybe Int]] -> (Int, (String, Int))
chooseCard f co                     = (fromJust best_idx, fromJust best_card) -- Slightly risky to just fromJust here.
    where card_stops                = Data.List.map cardWin co
          best_card                 = choose card_stops
          best_idx                  = elemIndex best_card card_stops
          choose xs                 = foldl (cardFold f) Nothing xs

cardScore                           :: [Int] -> Int -> [Int] -> Int
cardScore co cl_idx crd             = score
    where called_ns                 = take (cl_idx + 1) co
          uncalled_ns               = crd \\ called_ns -- list difference
          sum_uncalled              = sum uncalled_ns
          last_called               = last called_ns
          score                     = last_called * sum_uncalled


-- ----- Answer Functions

answer1                             :: [Int] -> [[Int]] -> (Int, String, Int)
answer1 co crds                     = (cd_idx, c_rc, score)
    where c_ords                    = cardIndices co crds
          (cd_idx, (c_rc, cl_idx))  = chooseCard (<) c_ords
          score                     = cardScore co cl_idx (crds !! cd_idx)

answer2                             :: [Int] -> [[Int]] -> (Int, String, Int)
answer2 co crds                     = (cd_idx, c_rc, score)
    where c_ords                    = cardIndices co crds
          (cd_idx, (c_rc, cl_idx))  = chooseCard (>) c_ords
          score                     = cardScore co cl_idx (crds !! cd_idx)
