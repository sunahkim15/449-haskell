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


getPenalties :: [Char] -> Char -> Int -> Int -> Int -> Char -> Int 
getPenalties  remaining previous currentMachine currentCost minLowerBound task
  |penalties !! (currentMachine) !! (charToInt (task)) == -1 = -1
  |currentCost + penalties !! (currentMachine) !! (charToInt (task)) > minLowerBound = -1
  --insert check for too near hard constraint here
  |otherwise --now we call subTreeLB again with updated currentCost, CurrentMachine, and remaining
  = subTreeLB
  (deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) --remove task from remaining
  (task)
  (currentMachine + 1)
  (currentCost + penalties !! (currentMachine) !! (charToInt (task)))
  (minLowerBound)

 
subTreeLB :: [Char] -> Char -> Int -> Int -> Int -> Int
subTreeLB  remaining previous currentMachine currentCost minLowerBound =
  minimum (minLowerBound:subTreePenalties)
  where   subTreePenalties = [y | y <- [getPenalties --need to remove -1 values here somewhere
                                        remaining
                                        previous
                                        currentMachine
                                        currentCost
                                        minLowerBound
                                        x
                                       | x <- remaining], not(y==(-1))]
                       
                       

           

main = do
  let prev = 'X'
      mach = 0
      currentCost = 0
      mlb = 99
      x = subTreeLB tasks prev mach currentCost mlb
  print x
        
