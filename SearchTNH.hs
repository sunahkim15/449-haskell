tooNearHard :: Char -> [Char]
tooNearHard parentTask = result
  where parent = charToInt parentTask
        result = [intToChar x | x <- [0..7], ((tooNearHardPA !! parent) !! x) == True]
