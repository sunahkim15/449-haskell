module Functions where
import Data.List
import Data.Maybe

data Assignment = Assignment {machine :: Int, task :: Char, penalty :: Int} deriving (Show)

intToChar :: Int -> Char
intToChar 0 = 'A'
intToChar 1 = 'B'
intToChar 2 = 'C'
intToChar 3 = 'D'
intToChar 4 = 'E'
intToChar 5 = 'F'
intToChar 6 = 'G'
intToChar 7 = 'H'
intToChar x = 'Z'

charToInt :: Char -> Int
charToInt 'A' = 0
charToInt 'B' = 1
charToInt 'C' = 2
charToInt 'D' = 3
charToInt 'E' = 4
charToInt 'F' = 5
charToInt 'G' = 6
charToInt 'H' = 7
charToInt x = 10

lowestMTpenalty :: Int -> [[Int]] -> Int
lowestMTpenalty m pa = minP (pa !! m)

getTaskFromPenalty :: Int -> Int -> [[Int]] -> Char
getTaskFromPenalty m p pa = intToChar(fromJust (elemIndex p (pa !! m)))

minP :: [Int] -> Int 
minP [] = error "empty list"
minP [x] = x  
minP (x:y:xs) = if (x == -1) then minP(y:xs) else if (y == -1) then minP(x:xs) else if (x < y) then minP(x:xs) else minP(y:xs)

--filterFalse :: [(Int, Bool)] -> [Maybe (Int, Bool)]
--filterFalse [(t, b)] = if b == True then [Nothing] else [(t, b)]
--filterFalse [(t, b):xs] = if b == True then filterFalse [xs] else filterFalse
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred 

addToPenaltyRow :: [Int] -> [(Int,Int)] -> [Int]
addToPenaltyRow pr [x] = addToPenaltyRow' pr (fst x) (snd x)
addToPenaltyRow pr (x:xs) = addToPenaltyRow (addToPenaltyRow' pr (fst x) (snd x)) (xs)

addToPenaltyRow' :: [Int] -> Int -> Int -> [Int]
addToPenaltyRow' pr t p = [if x == t then (pr !! x) + p else (pr !! x) | x <- [0..7], (pr !! x) /= -1]

