import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Maybe
import Data.Bool

-- Convert tuple to list
tupleToList :: (a, a) -> [a]
tupleToList (x, y) = [x, y]

-- Find the index of given element in an array
getIndex :: [String] -> String-> [Maybe Int]
getIndex lst str = 
    return (elemIndex str lst)

-- Split the list at given index
splitList :: [String] -> Int -> [([String], [String])]
splitList lst ind = do
    let parsedList = splitAt ind lst
    return parsedList

-- Parse string array at given index  
parseData :: [String] -> [[String]]
parseData lst = clst
    where softTooNearInd = fromJust $ head $ getIndex lst "too-near penalities"
          softTooNear = head (tail $ tupleToList $ head $ splitList lst softTooNearInd)   
          temp = head $ tupleToList $ head $ splitList lst softTooNearInd

          mPenaltiesInd = fromJust $ head $ getIndex temp  "machine penalties:"
          mPenalties = head (tail $ tupleToList $ head $ splitList temp mPenaltiesInd)
          temp2 = head $ tupleToList $ head $ splitList temp mPenaltiesInd

          hardTooNearInd = fromJust $ head $ getIndex temp2 "too-near tasks:"
          hardTooNear = head (tail $ tupleToList $ head $ splitList temp2 hardTooNearInd)
          temp3 = head $ tupleToList $ head $ splitList temp2 hardTooNearInd

          forbiddenInd = fromJust $ head $ getIndex temp3 "forbidden machine:"
          forbidden = head (tail $ tupleToList $ head $ splitList temp3 forbiddenInd)
          temp4 = head $ tupleToList $ head $ splitList temp3 forbiddenInd

          forcedInd = fromJust $ head $ getIndex temp4 "forced partial assignment:"
          forced = head (tail $ tupleToList $ head $ splitList temp4 forcedInd)
          clst' = forced : forbidden : hardTooNear : mPenalties : softTooNear : []
          clst = map tail clst'

-- Remove every empty string elements from array of strings 
removeEmptyString :: [String] -> [String]
removeEmptyString lst = lst''
    where lst' = filter (/=" ") lst
          lst'' = filter (/="") lst'

-- Remove '(', ')', ',' in string
removeNotChar :: String -> String
removeNotChar lst = newLst
    where lst' = delete ')' lst
          lst'' = delete '(' lst'
          newLst = filter (/=',') lst''

-- Convert string to corresponding integer value
stringToInt :: [String] -> [Int]
stringToInt lst = lst' 
    where lst' = [read x :: Int | x <- lst]

-- Parse value written in string to string array
listToArray :: [String] -> [[String]]
listToArray lst = tup
    where lst' = removeEmptyString lst
          lst'' = map removeNotChar lst' 
          tup = [words x | x <- lst''] 

-- Convert task to index 
stringToIndex :: String -> Int
stringToIndex str 
    | str == "A" = 0
    | str == "B" = 1
    | str == "C" = 2
    | str == "D" = 3
    | str == "E" = 4
    | str == "F" = 5
    | str == "G" = 6
    | otherwise = 7

-- Replace the element in (i, j) to given value 
-- Code is from: https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell 
replaceElem :: [[a]] -> a -> (Int, Int) -> [[a]]
replaceElem lst val (x, y) = 
    take x lst ++ [take y (lst !! x) ++ [val] ++ drop (y + 1) (lst !! x)] ++ drop (x + 1) lst

-- Get machine penalties from string array parsed from input file
getMachinePenalties :: [String] -> [[Int]]
getMachinePenalties lst = mp 
    where lst' = [words x | x <- lst]
          mp = [stringToInt x | x <- lst']         

-- Convert the string value to (task, task) 
getTaskTaskIndex :: [String] -> (Int, Int)
getTaskTaskIndex [x, y] = (x', y')
    where x' = stringToIndex x
          y' = stringToIndex y

-- Convert the string value to (mach, task) 
getMachTaskIndex :: [String] -> (Int, Int)
getMachTaskIndex [x, y] = (x', y')
    where x' = (read x :: Int) - 1
          y' = stringToIndex y

-- Apply forbidden constraint to machine penalties array
applyForbidden :: [[String]] -> [[Int]] -> Int -> [[Int]]
applyForbidden [] penalties n = penalties
applyForbidden forbidden penalties n
     | n < 0     = penalties
     | otherwise = applyForbidden forbidden penalties' (n-1)
    where penalties' = replaceElem penalties (-1) ind
          ind = tupleInd !! n
          tupleInd = [getMachTaskIndex x | x <- forbidden]

-- Get indices of elements that cannot be (because there is forced const.)
reverseIndex :: (Int, Int) -> [(Int, Int)]
reverseIndex (x, y) = indices
    where sameY = [(x', y) | x' <- [0,1..7], x' /= x]
          sameX = [(x, y') | y' <- [0,1..7], y' /= y]
          indices = sameX ++ sameY

-- Get all reverse indices given an array of forced elements
getAllIndices :: [[String]] -> [(Int, Int)]
getAllIndices forced = lst
    where indices = [getMachTaskIndex x | x <- forced]
          reversed = [reverseIndex y | y <- indices]
          lst = concat reversed

-- Apply forced constraint to penalty array
applyForced :: [[String]] -> [[Int]] -> Int -> [[Int]]
applyForced [] penalties n = penalties
applyForced forced penalties n 
     | n < 0     = penalties
     | otherwise = applyForced forced penalties' (n-1)
    where penalties' = replaceElem penalties (-1) ind
          ind = tupleInd !! n
          tupleInd = getAllIndices forced

-- Create penalty array applied with forced and forbidden constraints
getPenaltyArray :: [[String]] -> [[Int]]
getPenaltyArray lst = lst'
    where machinePenalties = getMachinePenalties (lst !! 3)
          forced = listToArray (lst !! 0) 
          forbidden = listToArray (lst !! 1) 
          temp = applyForbidden forbidden machinePenalties ((length forbidden) - 1)
          temp' = getAllIndices forced
          lst' = applyForced forced temp ((length temp') - 1) 

-- Create too-near hard constraints array
getHardTooNear :: [[String]] -> [[Bool]] -> Int -> [[Bool]] 
getHardTooNear [] lst n = lst
getHardTooNear hardTooNear lst n 
     | n < 0     = lst
     | otherwise = getHardTooNear hardTooNear lst' (n-1)
    where lst' = replaceElem lst True ind
          ind = tupleInd !! n
          tupleInd = [getTaskTaskIndex x | x <- hardTooNear]

getHardTooNear' :: [[String]] -> [[Bool]]
getHardTooNear' lst = lst'
    where hardTooNear = listToArray (lst !! 2)
          initList = createArray False
          lst' = getHardTooNear hardTooNear initList (length hardTooNear - 1)

-- Initializes 8 X 8 2D array
createArray :: a -> [[a]]
createArray val = lst
     where lst = replicate 8 (replicate 8 val)

-- Create too-near soft constraints array
getSoftTooNear :: [[String]] -> [[Int]] -> Int -> [[Int]]
getSoftTooNear [] lst n = lst
getSoftTooNear softTooNear lst n
     | n < 0     = lst
     | otherwise = getSoftTooNear softTooNear lst' (n-1)
    where lst' = replaceElem lst val ind
          val = penalties !! n
          penalties' = [last x | x <- softTooNear]
          penalties = [read x :: Int | x <- penalties']
          ind = tupleInd !! n
          tupleInd' = [init x | x <- softTooNear]
          tupleInd = [getTaskTaskIndex x | x <- tupleInd']

getSoftTooNear' :: [[String]] -> [[Int]]
getSoftTooNear' lst = lst'
    where softTooNear = listToArray (lst !! 4)
          initList = createArray 0
          lst' = getSoftTooNear softTooNear initList (length softTooNear - 1)
               
-- Check if given machine penalty is valid
validMachPenalty :: [[Int]] -> Bool
validMachPenalty lst = x && y
    where x = length lst == 8
          y = checkElem lst 7

-- Check if all the elements of machine penalty array has length 8
checkElem :: [[Int]] -> Int -> Bool
checkElem lst n
     | n < 0     = True
     | otherwise = (checkElem lst (n-1)) && (length lst' == 8) 
    where lst' = lst !! n

-- Check if tasks of soft too-near constraints are valid
validTask :: [[String]] -> Bool
validTask lst = val
    where val = validTask' lst' n
          lst' = listToArray (lst !! 4)
          n = length lst' - 1

validTask' :: [[String]] -> Int -> Bool
validTask' [] n = True
validTask' soft n 
     | n < 0     = True
     | otherwise = (validTask' soft (n-1)) && x
    where x = y && z
          y = (lst !! 0) `elem` tasks
          z = (lst !! 1) `elem` tasks
          lst = soft !! n
          tasks = ["A", "B", "C", "D", "E", "F", "G", "H"]

-- Check if penalties for soft constraints are all natural numbers
validPenalty :: [[String]] -> Bool
validPenalty lst = val
    where val = validMach && validSoft
          validMach = validPenalty' mp 7
          validSoft = validPenalty'' sp sLength
          mp = getMachinePenalties (lst !! 3) 
          sp = listToArray (lst !! 4)  
          sLength = length sp - 1 

-- Check if penalties in machine penalty array are natural numbers
validPenalty' :: [[Int]] -> Int -> Bool
validPenalty' lst n 
     | n < 0     = True
     | otherwise = (validPenalty' lst (n-1)) && (naturalNum lst' 7)
    where lst' = lst !! n

-- Check if all the elements in an integer array are natural numbers
naturalNum :: [Int] -> Int -> Bool
naturalNum lst n
     | n < 0     = True
     | otherwise = (naturalNum lst (n-1)) && (x > (-1))
    where x = lst !! n

-- Check if third elements of soft too-near constraints are natural numbers
validPenalty'' :: [[String]] -> Int -> Bool
validPenalty'' [] n = True
validPenalty'' soft n 
     | n < 0     = True
     | otherwise = (validPenalty'' soft (n-1)) && x
    where x = y > (-1)
          y = read z :: Int
          z = lst !! 2
          lst = soft !! n

-- Check if machine and task of hard constraints are valid
invalidMachTask :: [[String]] -> Bool  
invalidMachTask lst = validMachines && validTasks 
    where validMachines = checkMach lst
          validTasks = checkTask lst

-- Check if machines are valid
checkMach :: [[String]] -> Bool
checkMach lst = validForced && validForbidden
    where validForced = checkMach' forced fl
          validForbidden = checkMach' forbidden fl'
          forced = listToArray (lst !! 0) 
          forbidden = listToArray (lst !! 1)
          fl = length forced - 1
          fl' = length forbidden - 1

checkMach' :: [[String]] -> Int -> Bool
checkMach' [] n = True
checkMach' lst n 
     | n < 0     = True
     | otherwise = (checkMach' lst (n-1)) && x
    where x = y `elem` machines
          y = lst' !! 0
          lst' = lst !! n
          machines = ["1", "2", "3", "4", "5", "6", "7", "8"]

-- Check if tasks are valid
checkTask :: [[String]] -> Bool
checkTask lst = validForced && validForbidden && validHardTooNear
    where validForced = checkTask' forced fl
          validForbidden = checkTask' forbidden fl'
          forced = listToArray (lst !! 0) 
          fl = length forced - 1
          forbidden = listToArray (lst !! 1)
          fl' = length forbidden - 1
          validHardTooNear = checkTask'' hardTooNear ht
          hardTooNear = listToArray (lst !! 2) 
          ht = length hardTooNear - 1

checkTask' :: [[String]] -> Int -> Bool
checkTask' [] n = True
checkTask' lst n
     | n < 0     = True
     | otherwise = (checkTask' lst (n-1)) && x
    where x = y `elem` tasks
          y = lst' !! 1
          lst' = lst !! n
          tasks = ["A", "B", "C", "D", "E", "F", "G", "H"]

checkTask'' :: [[String]] -> Int -> Bool
checkTask'' [] n = True
checkTask'' lst n
     | n < 0     = True
     | otherwise = (checkTask'' lst (n-1)) && x
    where x = y && z
          y = y' `elem` tasks
          z = z' `elem` tasks
          y' = lst' !! 0
          z' = lst' !! 1
          lst' = lst !! n
          tasks = ["A", "B", "C", "D", "E", "F", "G", "H"]

-- Check if there exists a partial assignment that is invalid
partialError :: [[String]] -> Bool
partialError lst = partialError' lst && partialError'' lst

-- Check if there are pairs with the same machine or the same task
partialError' :: [[String]] -> Bool
partialError' lst = uniqueTasks && uniqueMachines
    where forced = listToArray (lst !! 0) 
          forcedTasks = [head x | x <- forced]
          forcedMachines = [last x | x <- forced]
          uniqueTasks = allUnique forcedTasks
          uniqueMachines = allUnique forcedMachines

allUnique :: [String] -> Bool
allUnique [] = True
allUnique (x:xs) = (allUnique xs) && (x `notElem` xs)

partialError'' :: [[String]] -> Bool
partialError'' lst = isNotSame forced forbidden n
    where forced = listToArray (lst !! 0)
          forbidden = listToArray (lst !! 1)
          n = length forced - 1

isNotSame :: [[String]] -> [[String]] -> Int -> Bool
isNotSame [] _ _ = True
isNotSame forced forbidden n
     | n < 0     = True
     | otherwise = (x `notElem` forbidden) && isNotSame forced forbidden (n-1)
    where x = forced !! n

-- Check if all the inputs are valid and returns false if they aren't
isValidInput :: [[String]] -> Bool
isValidInput lst
     | isValidPartial == False        = False
     | isValidMachTask == False       = False
     | machinePenaltyIsValid == False = False
     | isValidTasks == False          = False
     | isValidPenalty == False        = False
     | otherwise                      = True
    where isValidPartial = partialError lst
          isValidMachTask = invalidMachTask lst
          mp = getMachinePenalties (lst !! 3)
          machinePenaltyIsValid = validMachPenalty mp
          isValidTasks = validTask lst
          isValidPenalty = validPenalty lst

-- Get error message to print to output file
-- If there is no error, message will be "No error"
getErrorMessage :: [[String]] -> String
getErrorMessage lst 
     | isValid == False = getErrorMessage' lst
     | otherwise        = "No error" 
    where isValid = isValidInput lst

getErrorMessage' :: [[String]] -> String
getErrorMessage' lst
     | isValidPartial == False        = "partial assignment error"
     | isValidMachTask == False       = "invalid machine/task"
     | machinePenaltyIsValid == False = "machine penalty error"
     | isValidTasks == False          = "invalid task"
     | isValidPenalty == False        = "invalid penalty"
    where isValidPartial = partialError lst
          isValidMachTask = invalidMachTask lst
          mp = getMachinePenalties (lst !! 3)
          machinePenaltyIsValid = validMachPenalty mp
          isValidTasks = validTask lst
          isValidPenalty = validPenalty lst

main = do
    (inputFile:outputFile:_) <- getArgs
    contents <- readFile inputFile
    let linesOfFile = lines contents
    let constList = map removeEmptyString (parseData linesOfFile)
    
    -- Get the error message and write it to output file
    -- If there constraints are valid, it will be overwritten with solution
    let errorMessage = getErrorMessage constList
    writeFile outputFile errorMessage
 
    let penaltyArray = getPenaltyArray constList
    let hardTooNear = getHardTooNear' constList
    let softTooNear = getSoftTooNear' constList    



--    print penaltyArray
--    print hardTooNear
--    print softTooNear