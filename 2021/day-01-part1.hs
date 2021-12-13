import System.IO

main = do
    f <- readFile "day-01-input.txt"
    -- Get the lines and convert to integer
    let l = map read (lines f)::[Integer]
    -- Zip up the lines with itself (minus one) to compare pairs
    let p = zip l (drop 1 l)
    -- Compare pairs and get a list of integers
    let c = map pair_greater p
    -- Sum the integers
    print (sum c)

pair_greater (a, b) = (if a < b then 1 else 0)
