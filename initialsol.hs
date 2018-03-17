data Assignment = Assignment {machine :: Int, task :: Char, penalty :: Int} deriving (Show)

let penaltyarray = [[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]]
let machinetemp = 0
--we need to find task for machine 0
--get best task for machine 0
let tasktemp = machinetemp (bestMachineMatch machinetemp penaltyarray) penaltyarray
let m0 = Assignment machinetemp tasktemp

--too near hard constraint function; returns a list of tuples (task, boolean)
--takes in list of available tasks, the 2d pa for too near hard, and an assignment
tooNearHard :: [Int] -> [[Bool]] -> Assignment -> [(Int,Bool)]
tooNearHard at pa a = result
    where task1 = task a
          taskList = availTask
          result = [(x, ((pa !! task1) !! x))| x <- taskList)]

initsol :: Int -> [Assignment] -> [[Int]] -> [[Bool]] -> [[Int]] -> [Assignment]
initsol 6 h pa th ts =
--available task (task to int)
let machinetemp = 6
let allTask = [0,1,2,3,4,5,6,7]
let availTask = filter not(`elem`(map chartoInt (map task h))) allTask
--function find best task for machine 6
    --calc penalties previous
let m = map machine h
let t = map task h
--note*****might be good to put penalty in assignment
	--get row of penalty for machine 6 from pa
let pr = pa !! machinetemp
	--for each penalty task for machine 6
	    --check too near hard constraint for task (task for 6,7 and 7,0)
      --if true,
		--if false add any too near soft constraint to penalty of task
	--use minP to get task for machine 6
	--create machine 6 Assignment and put in history
	--machinetemp ++
	--call initsol machinetemp h pa th ts
initsol 7 h pa th ts =
--same as above but at end return history
initsol m h pa th ts =
--same as 6 but check one too near hard/soft constraint
