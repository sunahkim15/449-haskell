import qualified Functions
import Functions.Assignment

initsol :: Int -> [Assignment] -> [[Int]] -> [[Bool]] -> [[Int]] -> [Assignment]
initsol 6 h pa th ts = result
--available task (task to int)
  where machinetemp = 6
        allTask = [0,1,2,3,4,5,6,7]
        availTask = filter (not(`elem`(map chartoInt (map task h)))) allTask
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
        --if false add any too near soft constraint to penalty of task
        listTNS = getTooNearSoft listTNHfalse (h !! 5) ts
        penaltyRow = pa !! machinetemp
        penaltyRowAdded = addToPenaltyRow penaltyRow listTNS
        --use minP to get task for machine 6
        availT = map fst listTNS
        availPenalty = [(penaltyRowAdded !! x)| x <- availT]
        minPenalty = minP availPenalty
        theTask = [x | x <- [0..7], (penaltyRowAdded !! x) == minPenalty, elem x availT]
    --create machine 6 Assignment and put in history
        m6 = Assignment machinetemp (intToChar(theTask !! 0)) minPenalty
        machinetemp' = succ machinetemp 
        newh = h ++ [m6]
    --call initsol machinetemp h pa th ts 
        result = initsol machinetemp' newh pa th ts 
--initsol 7 h pa th ts = 
--let machinetemp = 7
--same as above but at end return history 
--initsol m h pa th ts = 
--same as 6 but check one too near hard/soft constraint
