data Node = Node { parent :: Node,
					machine :: Int,
					task :: Char,
					children :: [Node]
					} deriving (Show)
					
-- Take out the tasks that are already taken from the availableTasks array
-- params: availableTasks, takenTasks
-- return: availableTasks updated
getAvailTasks :: [Char] -> [Char] -> [Char]
getAvailTasks a [] = a
getAvailTasks a (y:ys) = getAvailTasks (secondLoop a y) ys

secondLoop ::[Char] -> Char -> [Char]
secondLoop [] _ = []
secondLoop (x:xs) y = thirdLoop x y ++ secondLoop xs y 

thirdLoop :: Char -> Char -> [Char]
thirdLoop x y   | x == y = []
                | otherwise = [x]
---------------------------------------------------------------------------

----------------------------- create children -----------------------------
-- params: parentNode, availableTasks, takenTasks
createChildren ::  Int -> Node -> [Char] -> [Char] -> [Node]
createChildren _ _ [] [] = []
createChildren pn at tt = if (machine pn) == -1 
                            then do
								 
								 
			  else if (machine pn) == 6
			     then do
							
			  else do
