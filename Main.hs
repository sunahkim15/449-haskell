module Main where

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

penalties = [[1,-1,-1,-1,-1,-1,-1,-1],
             [-1,1,-1,-1,-1,-1,-1,-1],
             [-1,-1,1,-1,-1,-1,-1,-1],
             [-1,-1,-1,1,-1,-1,-1,-1],
             [-1,-1,-1,-1,1,-1,-1,-1],
             [-1,-1,-1,-1,-1,1,10,20],
             [-1,-1,-1,-1,-1,-1,10,30],
             [-1,-1,-1,-1,-1,10,1,1]]

                
tooNearSoft =[[0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0]]
              

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
tooNearHard = [[False,False,False,False,False,False,False,False],
               [False,False,False,False,False,False,False,False],
               [False,False,False,False,False,False,False,False],
               [False,False,False,False,False,False,False,False],
               [False,False,False,False,False,True,False,False],
               [False,False,False,False,True,False,False,False],
               [False,False,False,False,False,False,False,False],
               [False,False,False,False,False,False,False,False]]            


tasks = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']


getPenalties :: [Char] -> [Char] -> Char -> Int -> Int -> Char -> (Int,[Char])
getPenalties  assigned remaining previous currentMachine currentCost task
  |penalties !! (currentMachine) !! (charToInt (task)) == -1 = (-1,['X'])
  |not(currentMachine == 0) && elem task (getTooNearHard previous) = (-1,['X'])
  |(deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) == [] &&  elem (head assigned) (getTooNearHard task) = (-1,['X'])
  |(deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) == [] = (((getTooNearSoft previous task) +
                                                                           (getTooNearSoft task (head assigned)) +
                                                                           currentCost +
                                                                           penalties !! (currentMachine) !! (charToInt (task))), assigned++[task]) 
  --insert check for too near hard constraint here
  |otherwise --now we call subTreeLB again with updated currentCost, CurrentMachine, and remaining
  = subTreeLB
   (assigned++[task]) --add task to assigned list
   (deleteN(eliminate(elemIndex(task) (remaining))) (remaining)) --remove task from remaining
   (task)
   (currentMachine + 1)
   ((getTooNearSoft previous task) + currentCost + penalties !! (currentMachine) !! (charToInt (task)))




subTreeLB ::  [Char] -> [Char] -> Char -> Int -> Int -> (Int,[Char])
subTreeLB  assigned remaining previous currentMachine currentCost  = getMinimumSubTreePenalties subTreePenalties
  where   subTreePenalties = [getPenalties --need to remove -1 values here somewhere
                               assigned
                               remaining
                               previous
                               currentMachine
                               currentCost
                               task
                             | task <- remaining]

------------------- getMinimumSubTreePenalties -----------------------------------------------------------------------------------------
getMinimumSubTreePenalties :: [(Int, [Char])] -> (Int, [Char])
getMinimumSubTreePenalties listOfSols
  |[x | x <- listOfSols, not(fst x == -1)] == [] = (-1,['X'])
  |otherwise =  listOfSols !! (fromJust (elemIndex (minimum [sol | sol <- solutions, not (sol == (-1))]) solutions))
  where solutions = [fst sol | sol <- listOfSols]

------------------- getTooNearSoft -----------------------------------------------------------------------------------------------------
getTooNearSoft :: Char -> Char -> Int
getTooNearSoft parent child
  |parent == 'X' = 0
  |otherwise = (tooNearSoft !! (charToInt parent)) !! (charToInt child)

getTooNearHard :: Char -> [Char]
getTooNearHard parentTask = result
  where parent = charToInt parentTask
        result = [intToChar x | x <- [0..7], ((tooNearHard !! parent) !! x) == True]

main = do
  let assigned = []
      remaining = tasks
      previous = 'X'
      currentMachine = 0
      currentCost = 0
      x = subTreeLB assigned tasks previous currentMachine currentCost
  print x

