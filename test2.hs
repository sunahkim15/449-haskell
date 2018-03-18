import Data.List
import Data.Maybe

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

penalties = [[10,10,10,10,10,10,10,10],
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
  |(deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) == [] = (currentCost + penalties !! (currentMachine) !! (charToInt (task)))
  --insert check for too near hard constraint here
  |otherwise --now we call subTreeLB again with updated currentCost, CurrentMachine, and remaining
  = subTreeLB
   (deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) --remove task from remaining
   (task) 
   (currentMachine + 1)
   (currentCost + penalties !! (currentMachine) !! (charToInt (task)))
   (minLowerBound)

 
subTreeLB :: [Char] -> Char -> Int -> Int -> Int -> Int
subTreeLB  remaining previous currentMachine currentCost minLowerBound = minimum subTreePenalties
  where   subTreePenalties = [penalty | penalty <- [getPenalties --need to remove -1 values here somewhere
                                                    remaining
                                                    previous
                                                    currentMachine
                                                    currentCost
                                                    minLowerBound
                                                    task
                                                   | task <- remaining],
                               not(penalty==(-1))]
                               
------------------- getMinimumSubTreePenalties -----------------------------------------------------------------------------------------
getMinimumSubTreePenalties :: [(Int, [Char])] -> (Int, [Char])
getMinimumSubTreePenalties listOfSols = listOfSols !! (fromJust (elemIndex (minimum [sol | sol <- solutions, not (sol == (-1))]) solutions))
  where solutions = [fst sol | sol <- listOfSols]
                       

main = do
  let remaining = tasks
      previous = 'T'
      currentMachine = 0
      currentCost = 0
      minLowerBound = 100
      x = subTreeLB tasks previous currentMachine currentCost minLowerBound
  print x
