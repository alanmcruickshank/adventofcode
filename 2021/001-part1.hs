import System.IO

main = do
    f <- readFile "001-input.txt"
    -- Get the lines and convert to integer
    let l = map read (lines f)::[Integer]
    -- Zip up the lines with itself (minus one) to compare pairs
    let p = zip l (drop 1 l)
    let n = length p
    print n
    print (head p)
    let c = map pair_greater p
    print (head c)
    let r = sum c
    print r

pair_greater (a, b) = (if a < b then 1 else 0)
