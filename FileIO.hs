import System.IO
import System.Environment
import Data.List
import Data.Maybe

forcedPartial = [(Int, Char)]

-- Returns the index of string
getIndex :: [String] -> String -> [Maybe Int]
getIndex lst str = do
    return (elemIndex str lst)

-- Returns the parsed list  
splitList :: [String] -> Int -> [([String],[String])]
splitList lst ind = do
    let parsedList = splitAt ind lst
    return parsedList

data listofTuples_IntChar = [(Int, Char)]
data listofTuples_CharChar = [(Char, Char)]
data listof8Ints = [(Int, Int, Int, Int, Int, Int, Int, Int)]
data listofTriples_CharCharInt = [(Char, Char, Int)]

main = do
    (inputFile:outputFile:_) <- getArgs
    contents <- readFile inputFile
    let linesOfFile = lines contents 
    
    let softTooNearIndex = fromJust $ head $ getIndex linesOfFile "too-near penalities"
    let softTooNear = head (tail $ tupleToList $ head $ splitList linesOfFile softTooNearIndex)
    let temp = head $ tupleToList $ head $ splitList linesOfFile softTooNearIndex
    
    let machinePenaltiesIndex = fromJust $ head $ getIndex temp "machine penalties:"
    let machinePenalties = head (tail $ tupleToList $ head $ splitList temp machinePenaltiesIndex)  
    let temp2 = head $ tupleToList $ head $ splitList temp machinePenaltiesIndex

    let hardTooNearIndex = fromJust $ head $ getIndex temp2 "too-near tasks:"
    let hardTooNear = head (tail $ tupleToList $ head $ splitList temp2 hardTooNearIndex)
    let temp3 = head $ tupleToList $ head $ splitList temp2 hardTooNearIndex
    
    let forbiddenIndex = fromJust $ head $ getIndex temp3 "forbidden machine:"
    let forbidden = head (tail $ tupleToList $ head $ splitList temp3 forbiddenIndex)
    let temp4 = head $ tupleToList $ head $ splitList temp3 forbiddenIndex
    
    let forcedIndex = fromJust $ head $ getIndex temp4 "forced partial assignment:"
    let forced = head (tail $ tupleToList $ head $ splitList temp4 forcedIndex)

    print forced
    print forbidden
    print hardTooNear
    print machinePenalties
    print softTooNear

    
    
	
	forcedPartial::listofTuples_IntChar
	forbideenMachine::listofTuples_IntChar
	tooNearTasks::listofTuples_CharChar
	machinePenalties::listof8Ints
	tooNearPenalties::listofTriples_CharCharInt
	
-- function to parse the contents into data types
parseContents :: Char[] -> listofTuples_IntChar -> listofTuples_IntChar -> listofTuples_CharChar -> listof8Ints -> listofTriples_CharCharInt
