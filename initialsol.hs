data Assignment = Assignment {machine :: Int, task :: Char} deriving (Show)

let penaltyarray = [[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,
[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]] 

  
let machinetemp = 0
--we need to find task for machine 0

let tasktemp = bestMachineMatch machinetemp penaltyarray
 