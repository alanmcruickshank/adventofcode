import System.IO

main = do
    f <- readFile "001-input.txt"
    let l = lines f
    let n = length l
    print n
