
data Assignment = Assignment {machine :: Int, task :: Char, penalty :: Int} deriving (Show)

initsol :: Int -> [Assignment] -> [[Int]] -> [[Bool]] -> [[Int]] -> [Assignment]
initsol 6 h pa th ts = 
--available task (task to int)
let machinetemp = 6
    allTask = [0,1,2,3,4,5,6,7]
    availTask = filter not(`elem`(map chartoInt (map task h))) allTask
--function find best task for machine 6 
    --calc penalties previous (penalties previous includes toonear penalties)
    prePenalties = sum (map penalty h)
    m = map machine h
    t = map task h
    --for each penalty task for machine 6 
        --check too near hard constraint for task (task for 6,7 and 7,0)
    listTNHfalse = filterNot snd listTNH 
        --if false add any too near soft constraint to penalty of task
    penaltyRow = pa !! machinetemp
    penaltyRowAdded = [if x == fst y then (penaltyRow !! x) + snd y else (penaltyRow !! x)| x <- 0 .. 7, y <- listTNS]
    --use minP to get task for machine 6
    --create machine 6 Assignment and put in history
    --machinetemp ++
    --call initsol machinetemp h pa th ts 
initsol 7 h pa th ts = 
let machinetemp = 7
--same as above but at end return history 
initsol m h pa th ts = 
--same as 6 but check one too near hard/soft constraint
