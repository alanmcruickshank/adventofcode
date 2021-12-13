import System.IO

main = do
    f <- readFile "002-input.txt"
    -- Get the lines and convert to vectors
    let l = map instruction_to_vector (lines f)
    print (head l)
    -- Fold the list by adding them, starting at 0,0
    let r = foldr vector_add (0, 0) l
    print r
    print (vector_self_product r)

-- Translate directions to unit vectors
unit_vector             :: String -> (Integer, Integer)
unit_vector "forward"   = (1, 0)
unit_vector "down"      = (0, 1)
unit_vector "up"        = (0, -1)

vector_mult             :: Integer -> (Integer, Integer) -> (Integer, Integer)
vector_mult a (x, y)    = (x * a, y * a)

vector_add                      :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
vector_add (x1, y1) (x2, y2)    = (x1 + x2, y1 + y2)

vector_self_product (x, y)      = x * y

instruction_to_list     :: String -> [String]
instruction_to_list s   = take 2 (words s)

list_to_vector          :: [String] -> (Integer, Integer)
list_to_vector [a, x]   = vector_mult (read x::Integer) (unit_vector a) 

instruction_to_vector   :: String -> (Integer, Integer)
instruction_to_vector   = list_to_vector . instruction_to_list
