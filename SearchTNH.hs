tooNearHard :: Char -> [Char]
tooNearHard parentTask = result
  where result = [x | x <- tasks, (penalties !! parentTask) !! x) == True] 
  
