module Main where

import System.IO
import System.Exit
import System.Environment
import Data.Bool
import FileIO
import Functions
import Data.List
import Data.Maybe

eliminate :: Maybe a -> a
eliminate (Just a) = a

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

  
              

  {-
tooNearHard = [[True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True],
               [True,True,True,True,True,True,True,True]]
-}


tasks = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']


getPenalties :: [Char] -> [Char] -> Char -> Int -> Int -> Char -> [[Int]] -> [[Bool]] -> [[Int]] -> (Int,[Char])
getPenalties  assigned remaining previous currentMachine currentCost task penalties tooNearHard tooNearSoft
  |penalties !! (currentMachine) !! (charToInt (task)) == -1 = (-1,['X'])
  |not(currentMachine == 0) && elem task (getTooNearHard previous tooNearHard) = (-1,['X'])
  |(deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) == [] &&  elem (head assigned) (getTooNearHard task tooNearHard) = (-1,['X'])
  |(deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) == [] = (((getTooNearSoft previous task tooNearSoft) +
                                                                           (getTooNearSoft task (head assigned) tooNearSoft) +
                                                                           currentCost +
                                                                           penalties !! (currentMachine) !! (charToInt (task))), assigned++[task]) 
  --insert check for too near hard constraint here
  |otherwise --now we call subTreeLB again with updated currentCost, CurrentMachine, and remaining
  = subTreeLB
   (assigned++[task]) --add task to assigned list
   (deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) --remove task from remaining
   (task)
   (currentMachine + 1)
   ((getTooNearSoft previous task tooNearSoft) + currentCost + penalties !! (currentMachine) !! (charToInt (task)))
   (penalties)
   (tooNearHard)
   (tooNearSoft)




subTreeLB ::  [Char] -> [Char] -> Char -> Int -> Int -> [[Int]]-> [[Bool]] -> [[Int]] -> (Int,[Char])
subTreeLB  assigned remaining previous currentMachine currentCost penalties tooNearHard tooNearSoft = getMinimumSubTreePenalties subTreePenalties
  where   subTreePenalties = [getPenalties --need to remove -1 values here somewhere
                               assigned
                               remaining
                               previous
                               currentMachine
                               currentCost
                               task
                               penalties
                               tooNearHard
                               tooNearSoft
                             | task <- remaining]

------------------- getMinimumSubTreePenalties -----------------------------------------------------------------------------------------
getMinimumSubTreePenalties :: [(Int, [Char])] -> (Int, [Char])
getMinimumSubTreePenalties listOfSols
  |[x | x <- listOfSols, not(fst x == -1)] == [] = (-1,['X'])
  |otherwise =  listOfSols !! (fromJust (elemIndex (minimum [sol | sol <- solutions, not (sol == (-1))]) solutions))
  where solutions = [fst sol | sol <- listOfSols]

------------------- getTooNearSoft -----------------------------------------------------------------------------------------------------
getTooNearSoft :: Char -> Char -> [[Int]] -> Int
getTooNearSoft parent child tooNearSoft
  |parent == 'X' = 0
  |otherwise = (tooNearSoft !! (charToInt parent)) !! (charToInt child)

getTooNearHard :: Char -> [[Bool]] -> [Char]
getTooNearHard parentTask tooNearHard = result
  where parent = charToInt parentTask
        result = [intToChar x | x <- [0..7], ((tooNearHard !! parent) !! x) == True]

main = do
  (inputFile:outputFile:_) <- getArgs
  contents <- readFile inputFile
  let assigned = []
      linesOfFile = lines contents
      constList = map removeEmptyString (parseData linesOfFile)
      message = getErrorMessage constList
  
  if message == "No error"
    then do let penalties = getPenaltyArray constList
                tooNearHard = getHardTooNear' constList
                tooNearSoft = getSoftTooNear' constList    
                remaining = tasks
                previous = 'X'
                currentMachine = 0
                currentCost = 0
                x = subTreeLB assigned tasks previous currentMachine currentCost penalties tooNearHard tooNearSoft
                taskString = intersperse ' ' ((snd x))
                qualString = show (fst x)
                solString = "Solution " ++ taskString ++ "; Quality: " ++ qualString
                nosol = "No valid solution possible!"
            if (fst x) == (-1) then do writeFile outputFile nosol
              else do writeFile outputFile solString
    else do writeFile outputFile message
   
