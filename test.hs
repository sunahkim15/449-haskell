import Data.List


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

eliminate :: Maybe a -> a
eliminate (Just a) = a

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

penalties = [[10,1,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10],
             [10,10,10,10,10,10,10,10]]
tasks = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']


search :: [Char] -> [Char] -> Char -> Int -> Int -> Int -> Int
search assigned remaining previous machine lowerBound minLowerBound = 1
  


dfs :: [Char] -> [Char] -> Char -> Int -> Int -> Int -> Int
dfs assigned remaining previous machine lowerBound minLowerBound
            | remaining == [] && lowerBound <= minLowerBound = lowerBound
            | remaining == [] && lowerBound > minLowerBound = minLowerBound
            | penalties !! (machine) !! (charToInt (remaining !! 0)) + lowerBound < minLowerBound
              && (not (elem (remaining !! 0) (assigned)))
              && not(penalties !! (machine) !! (charToInt (remaining !! 0)) == -1)
            = dfs
              (assigned ++ [remaining !! 0])
              (deleteN(eliminate(elemIndex(remaining !!0) (remaining))) (remaining))
              (head (remaining))
              (machine + 1)
              ((penalties !! (machine) !! (charToInt (remaining !! 0))) + lowerBound)
              (minLowerBound)
            |otherwise
            = dfs
              (assigned)
              (tail(remaining))
              (previous)
              (machine)
              (lowerBound)
              (minLowerBound)
main = do 
  let assigned = ['T']
      prev = 'X'
      mach = 0
      lb = 0
      mlb = 99
      x = dfs assigned tasks prev mach lb mlb
      y = dfs assigned tasks prev mach lb y
  print y
        
