import System.IO
import System.Environment

forcedPartial = [(Int, Char)]

data listofTuples_IntChar = [(Int, Char)]
data listofTuples_CharChar = [(Char, Char)]
data listof8Ints = [(Int, Int, Int, Int, Int, Int, Int, Int)]
data listofTriples_CharCharInt = [(Char, Char, Int)]

main = do
    (inputFile:outputFile:_) <- getArgs
    contents <- readFile inputFile
    putStrLn contents
	
	forcedPartial::listofTuples_IntChar
	forbideenMachine::listofTuples_IntChar
	tooNearTasks::listofTuples_CharChar
	machinePenalties::listof8Ints
	tooNearPenalties::listofTriples_CharCharInt
	
-- function to parse the contents into data types
parseContents :: Char[] -> listofTuples_IntChar -> listofTuples_IntChar -> listofTuples_CharChar -> listof8Ints -> listofTriples_CharCharInt
