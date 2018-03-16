-- note: DON'T KNOW IF THIS WORKS OR NOT; HAVEN'T TESTED

-- a and b are type classes; for our case, a should be Int and b should be Char
-- For example, you can create a RoseTree this way:
-- ourTree = Node { machine = 1, task = A, children = [ Node { machine = 2, task = B, children = [] },
--                                                      Node { machine = 2, task = C, children = [ Node { machine = 3, task = D, children = []] } ]

data RoseTree a b = Node { machine :: a,
                           task :: b,
						   children :: [RoseTree a b] }
						   deriving Show
						   
data onePath a b = Child a b [RoseTree a b]
			   
data Paths a b = [onePath a b]

---------------------------- get the parent node ----------------------------
-- Get (create) the parent node;
-- note: he list of the children in the parent node that gets created is not in order;
-- For example, the children list may be like this: 
-- children = [sub tree for machine 2 task 1, sub tree for machine 2 task 4, sub tree for machine 2 task 2, ...]
-- params: The current subtree of the rose tree, the path
getParentNode :: (RoseTree a b, Paths a b) -> (RoseTree a b, Paths a b)
getParentNode (t, Child a b [ts]:bs) = (Node { machine = a, task = b, children = t:[ts] }, bs)

--------------------------- get available tasks ------------------------------
-- Take out the tasks that are already taken from the availableTasks array
-- params: availableTasks, takenTasks
-- return: availableTasks updated
getAvailTasks :: [Char] -> [Char] -> [Char]
getAvailTasks a [] = a
getAvailTasks a (y:ys) = getAvailTasks (secondLoop a y) ys

--------------------------- second loop --------------------------------------
secondLoop ::[Char] -> Char -> [Char]
secondLoop [] _ = []
secondLoop (x:xs) y = thirdLoop x y ++ secondLoop xs y 

--------------------------- third loop ---------------------------------------
thirdLoop :: Char -> Char -> [Char]
thirdLoop x y   | x == y = []
                | otherwise = [x]

---------------------------- create child loop -------------------------------
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
                            then pn ++ createChildLoop 0 pn pa at
				else if (machine pn) == 6
				then
							
				else
				
