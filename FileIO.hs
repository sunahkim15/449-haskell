import System.IO
import System.Environment
import Data.List
import Data.Maybe

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

-- Parse string array 
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
    | otherwise  = 7

-- Replace the element in (i, j) to given value 
-- Code is from: https://stackoverflow.com/questions/20156078/replacing-an-element-in-a-list-of-lists-in-haskell 
replaceElem :: [[a]] -> a -> (Int, Int) -> [[a]]
replaceElem lst val (y, x) = 
    take x lst ++ [take y (lst !! x) ++ [val] ++ drop (y + 1) (lst !! x)] ++ drop (x + 1) lst

-- Get machine penalties from string array parsed from input file
getMachinePenalties :: [String] -> [[Int]]
getMachinePenalties lst = mp 
    where lst' = [words x | x <- lst]
          mp = [stringToInt x | x <- lst']         

-- (task, task) 
getTaskTaskIndex :: [String] -> (Int, Int)
getTaskTaskIndex [x, y] = (x', y')
    where x' = stringToIndex x
          y' = stringToIndex y

-- (mach, task) 
getMachTaskIndex :: [String] -> (Int, Int)
getMachTaskIndex [x, y] = (x', y')
    where x' = (read x :: Int) - 1
          y' = stringToIndex y

-- Apply forbidden constraint to machine penalties array
applyForbidden :: [[String]] -> [[Int]] -> Int -> [[Int]]
applyForbidden forbidden penalties n
     | n < 0     = penalties
     | otherwise = applyForbidden forbidden penalties' (n-1)
    where penalties' = replaceElem penalties (-1) ind
          ind = tupleInd !! n
          tupleInd = [getMachTaskIndex x | x <- forbidden]

-- Get indices of elements that cannot be (becase there is forced const.)
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
getHardTooNear hardTooNear lst n 
     | n < 0     = lst
     | otherwise = getHardTooNear hardTooNear lst' (n-1)
    where lst' = replaceElem lst False ind
          ind = tupleInd !! n
          tupleInd = [getTaskTaskIndex x | x <- hardTooNear]

getHardTooNear' :: [[String]] -> [[Bool]]
getHardTooNear' lst = lst'
    where hardTooNear = listToArray (lst !! 2)
          initList = createArray True
          lst' = getHardTooNear hardTooNear initList (length hardTooNear - 1)

-- Initializes 8 X 8 2D array
createArray :: a -> [[a]]
createArray val = lst
     where lst = replicate 8 (replicate 8 val)

-- Create too-near soft constraints array
getSoftTooNear :: [[String]] -> [[Int]] -> Int -> [[Int]]
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
               
main = do
    (inputFile:_) <- getArgs
    contents <- readFile inputFile
    let linesOfFile = lines contents
    let constList = map removeEmptyString (parseData linesOfFile)

    let penaltyArray = getPenaltyArray constList
    let hardTooNear = getHardTooNear' constList
    let softTooNear = getSoftTooNear' constList    

    print penaltyArray
    print hardTooNear
    print softTooNear
    
    