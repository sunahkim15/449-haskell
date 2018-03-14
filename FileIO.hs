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
    
    -- Find the indices of strings 
    let forcedIndex = fromJust $ head $ getIndex linesOfFile "forced partial assignment:"
    let forbiddenIndex = fromJust $ head $ getIndex linesOfFile "forbidden machine:"
    let hardTooNearIndex = fromJust $ head $ getIndex linesOfFile "too-near tasks:"
    let machinePenaltiesIndex = fromJust $ head $ getIndex linesOfFile "machine penalties:"
    let softTooNearIndex = fromJust $ head $ getIndex linesOfFile "too-near penalities"
    
    
    
	
	forcedPartial::listofTuples_IntChar
	forbideenMachine::listofTuples_IntChar
	tooNearTasks::listofTuples_CharChar
	machinePenalties::listof8Ints
	tooNearPenalties::listofTriples_CharCharInt
	
-- function to parse the contents into data types
parseContents :: Char[] -> listofTuples_IntChar -> listofTuples_IntChar -> listofTuples_CharChar -> listof8Ints -> listofTriples_CharCharInt
