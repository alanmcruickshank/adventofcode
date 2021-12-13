import System.IO

main = do
    f <- readFile "day-02-input.txt"
    -- Get the lines and convert to instructions
    let l = map line_to_instruction (lines f)
    -- Fold the list by adding them, starting at 0,0
    -- Foldl means we start by combining z with the first element
    let r = foldl interpret_instruction (0, 0, 0) l
    print r
    print (vector_self_product r)

-- Interpret Instruction
interpret_instruction                               :: (Integer, Integer, Integer) -> (String, Integer) -> (Integer, Integer, Integer)
interpret_instruction (x, y, a) ("forward",   mag)  = (x + mag, y + (a * mag), a)
interpret_instruction (x, y, a) ("down",      mag)  = (x, y, a + mag)
interpret_instruction (x, y, a) ("up",        mag)  = (x, y, a - mag)

vector_self_product (x, y, a)      = x * y

line_to_list     :: String -> [String]
line_to_list s   = take 2 (words s)

list_to_instruction             :: [String] -> (String, Integer)
list_to_instruction [dir, mag]  = (dir, (read mag::Integer))

line_to_instruction         :: String -> (String, Integer)
line_to_instruction         =  list_to_instruction . line_to_list

