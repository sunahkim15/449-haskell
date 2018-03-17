--too near hard constraint function; returns a list of tuples (task, boolean)
--takes in list of available tasks, the 2d pa for too near hard, and an assignment
tooNearHard :: [Int] -> [[Bool]] -> Assignment -> [(Int,Bool)]
tooNearHard at pa a = result
    where task1 = task a
          taskList = availTask
          result = [(x, ((pa !! task1) !! x))| x <- taskList)]
