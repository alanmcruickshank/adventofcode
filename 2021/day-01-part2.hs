import System.IO

main = do
    f <- readFile "day-01-input.txt"
    -- Get the lines and convert to integer
    let l = map read (lines f)::[Integer]
    print (head l)
    -- Zip up the lines with itself twice to get triplets
    let t = zip3 l (drop 1 l) (drop 2 l)
    -- Sum the triples (why the normal sum function doesn't work here baffles me)
    let s = map sum_tuple t
    -- Make pairs from that list
    let p = zip s (drop 1 s)
    -- Compare pairs and get a list of integers
    let c = map pair_greater p
    -- Sum the integers
    print (sum c)

pair_greater (a, b) = (if a < b then 1 else 0)

sum_tuple (a, b, c) = a + b + c
