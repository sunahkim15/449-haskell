import Data.List
import Data.Maybe

data Assignment = Assignment {machine :: Int, task :: Char, penalty :: Int} deriving (Show)

intToChar :: Int -> Char
intToChar 0 = 'A'
intToChar 1 = 'B'
intToChar 2 = 'C'
intToChar 3 = 'D'
intToChar 4 = 'E'
intToChar 5 = 'F'
intToChar 6 = 'G'
intToChar 7 = 'H'
intToChar x = 'Z'

charToInt :: Char -> Int
charToInt 'A' = 0
charToInt 'B' = 1
charToInt 'C' = 2
charToInt 'D' = 3
charToInt 'E' = 4
charToInt 'F' = 5
charToInt 'G' = 6
charToInt 'H' = 7
charToInt x = 10

lowestMTpenalty :: Int -> [[Int]] -> Int
lowestMTpenalty m pa = minP (pa !! m)

getTaskFromPenalty :: Int -> Int -> [[Int]] -> Char
getTaskFromPenalty m p pa = intToChar(fromJust (elemIndex p (pa !! m)))

minP :: [Int] -> Int 
minP [] = error "empty list"
minP [x] = x  
minP (x:y:xs) = if (x == -1) then minP(y:xs) else if (y == -1) then minP(x:xs) else if (x < y) then minP(x:xs) else minP(y:xs)

--filterFalse :: [(Int, Bool)] -> [Maybe (Int, Bool)]
--filterFalse [(t, b)] = if b == True then [Nothing] else [(t, b)]
--filterFalse [(t, b):xs] = if b == True then filterFalse [xs] else filterFalse
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred 

addToPenaltyRow :: [Int] -> [(Int,Int)] -> [Int]
addToPenaltyRow pr [x] = addToPenaltyRow' pr (fst x) (snd x)
addToPenaltyRow pr (x:xs) = addToPenaltyRow (addToPenaltyRow' pr (fst x) (snd x)) (xs)

addToPenaltyRow' :: [Int] -> Int -> Int -> [Int]
addToPenaltyRow' pr t p = [if x == t then (pr !! x) + p else (pr !! x) | x <- [0..7], (pr !! x) /= -1]

tooNearHard :: [Int] -> [[Bool]] -> Assignment -> [(Int,Bool)]
tooNearHard at pa a = result
  where task1 = task a
        parentTask = charToInt task1
        taskList = at
        result = [(x, ((pa !! parentTask) !! x))| x <- taskList]
        
getTooNearSoft :: [(Int, Bool)] -> Assignment -> [[Int]] -> [(Int, Int)]
getTooNearSoft tnh p tnsP = zip [fst x | x <- tnh] [(tnsP !! fst x) !! snd x | x <- indices]
    where indices = [(charToInt(task p), fst x) | x <- tnh]
    
initsol :: Int -> [Assignment] -> [[Int]] -> [[Bool]] -> [[Int]] -> [Assignment]
initsol 6 h pa th ts = result
--available task (task to int)
  where machinetemp = 6
        allTask = [0,1,2,3,4,5,6,7]
        availTask = filterNot (`elem`(map charToInt (map task h))) allTask
--function find best task for machine 6 
    --calc penalties previous (penalties previous includes toonear penalties)
        --prePenalties = sum (map penalty h)
    --for each penalty task for machine 6 
        --check too near hard constraint for task (task for 5,6 and 7,0)
        listTNH56 = tooNearHard availTask th (h !! 5)
        listTNH70 = tooNearHard availTask th (h !! 0)
        listTNH56false = filterNot snd listTNH56
        listTNH70false = filterNot snd listTNH70
        listTNHfalse = [x| x <- listTNH70false, x `elem` listTNH56false]  
        nosol = null listTNHfalse 
        --if false add any too near soft constraint to penalty of task
        listTNS = getTooNearSoft listTNHfalse (h !! 5) ts
        penaltyRow = pa !! machinetemp
        penaltyRowAdded = addToPenaltyRow penaltyRow listTNS
        --use minP to get task for machine 6
        availT = map fst listTNS
        availPenalty = [(penaltyRowAdded !! x)| x <- availT]
        minPenalty = minP availPenalty
        nosol' = (minPenalty == -1)
        theTask = [x | x <- [0..7], (penaltyRowAdded !! x) == minPenalty, elem x availT]
    --create machine 6 Assignment and put in history
        m6 = Assignment machinetemp (intToChar(theTask !! 0)) minPenalty
        machinetemp' = succ machinetemp 
        newh = h ++ [m6]
    --call initsol machinetemp h pa th ts 
        result = if nosol || nosol' then h else initsol machinetemp' newh pa th ts 
initsol 7 h pa th ts = result
  where machinetemp = 7   
        allTask = [0,1,2,3,4,5,6,7]
        availTask = filterNot (`elem`(map charToInt (map task h))) allTask
        nosol = null availTask
        listTNHfalse = [((availTask !! 0), False)]
        listTNS67 = getTooNearSoft listTNHfalse (h !! 6) ts
        listTNS70 = getTooNearSoft listTNHfalse (h !! 0) ts
        penaltyRow = pa !! machinetemp
        penaltyRowAdded = addToPenaltyRow penaltyRow listTNS67
        penaltyRowAdded2 = addToPenaltyRow penaltyRow listTNS70
        availPenalty = [(penaltyRowAdded2 !! x)| x <- availTask]
        minPenalty = minP availPenalty
        nosol' = (minPenalty == -1)
        theTask = [x | x <- [0..7], (penaltyRowAdded2 !! x) == minPenalty, elem x availTask]
        m7 = Assignment machinetemp (intToChar(availTask !! 0)) minPenalty
        newh = h ++ [m7]
        result = if nosol || nosol' then h else newh
initsol m h pa th ts = result
  where allTask = [0,1,2,3,4,5,6,7]
        availTask = filterNot (`elem`(map charToInt (map task h))) allTask
        listTNH = tooNearHard availTask th (h !! (m-1))
        listTNHfalse = filterNot snd listTNH
        nosol = null listTNHfalse 
        listTNS = getTooNearSoft listTNHfalse (h !! (m-1)) ts
        penaltyRow = pa !! m
        penaltyRowAdded = addToPenaltyRow penaltyRow listTNS
        availT = map fst listTNS
        availPenalty = [(penaltyRowAdded !! x)| x <- availT]
        minPenalty = minP availPenalty
        nosol' = (minPenalty == -1)
        theTask = [x | x <- [0..7], (penaltyRowAdded !! x) == minPenalty, elem x availT]
        mx = Assignment m (intToChar(availTask !! 0)) minPenalty
        machinetemp' = succ m
        newh = h ++ [mx]
        result = if nosol || nosol' then h else initsol machinetemp' newh pa th ts 
--same as 6 but check one too near hard/soft constraint


