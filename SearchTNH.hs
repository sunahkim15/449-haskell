tooNearHard :: Char -> [Char]
tooNearHard parentTask = listOfChars
  where parent = charToInt parentTask
        result = [x | x <- (0..7), ((penalties !! parent) !! x) == True] 
        listOfChars = [intToChar c | c <- result]
        
        
  
