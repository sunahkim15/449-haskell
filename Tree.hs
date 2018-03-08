data Node = Node { parent :: Node,
					machine :: Int,
					task :: Char,
					children :: [Node]
					} deriving (Show)