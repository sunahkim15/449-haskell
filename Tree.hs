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

---------------------------- create child loop ----------------------------
-- params: counter, parentNode, penaltyArray, availableTasks
createChildLoop :: Int -> Node -> [Char] -> [Node]
createChildLoop i pn pa at = if i>= (length at) then []
				else if (pa !! ((machine pn)+1) !! convertInt (at !! i)) /= (-1)
				then
				-- create a child
				-- Node childNode = new Node(parent, parentMachine + 1, availableTasks[i]);
				-- edit the historical path of this child
				-- ArrayList<Character> childHistory = parent.getHistory();
				-- childHistory.add(availableTasks[i]);
				-- childNode.setHistory(childHistory);
				-- childrenArray.add(childNode);
				-- reloop createChildLoop i++ pn pa at
				else createChildLoop (i+1) pn pa at
							 

----------------------------- create children -----------------------------
-- params: penaltyArray, parentNode, availableTasks, takenTasks
createChildren ::  [[Char]] -> Int -> Node -> [Char] -> [Char] -> [Node]
createChildren [] _ _ [] [] = []
createChildren pa pn at tt = if (machine pn) == -1 
                            then createChildLoop 0 pn pa at
				else if (machine pn) == 6
				then
							
				else
				
