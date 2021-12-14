-- Advent of code. Day 4.

main = do
    f <- readFile "day-04-example.txt"
    print "=== Part 1"
    let sections = split_sections f
    let call_order = head sections
    let cards = tail sections
    print call_order
    print cards

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
