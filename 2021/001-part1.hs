import System.IO

main = do
    h <- openFile "001-input.txt" ReadMode
    var <- hGetLine h
    putStrLn var
    hClose h
