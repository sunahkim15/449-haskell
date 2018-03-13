import System.IO
import System.Environment

main = do
    (inputFile:outputFile:_) <- getArgs
    contents <- readFile inputFile
    putStrLn contents