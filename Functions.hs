data Node = Node {machine :: Int, task :: Char} deriving (Show)

intToChar :: Int -> Char
intToChar 0 = 'A'
intToChar 1 = 'B'
intToChar 2 = 'C'
intToChar 3 = 'D'
intToChar 4 = 'E'
intToChar 5 = 'F'
intToChar 6 = 'G'
intToChar 7 = 'H'

charToInt :: Char -> Int
charToInt 'A' = 0
charToInt 'B' = 1
charToInt 'C' = 2
charToInt 'D' = 3
charToInt 'E' = 4
charToInt 'F' = 5
charToInt 'G' = 6
charToInt 'H' = 7

bestMachineMatch :: Int -> [[Int]] -> Char
bestMachineMatch m pa = intToChar (minimum (pa !! m))

checkForbidden :: Int -> Int -> [[Int]] -> Bool
checkForbidden m t fa = if ((fa !! m) !! t) == -1 then True else False

map :: (Int -> Int -> [[Int]] -> Bool) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

--list of whats been assigned
--list of tasks available
--list of children
    --list of lowerbound from list of children using map
    --use to get child with lowest lowerbound
