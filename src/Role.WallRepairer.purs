module Role.WallRepairer (runWallRepairer, WallRepairerMemory, WallRepairer) where

import Prelude
import CommonActions (collectEnergy, repairStructure)
import CreepRoles (Role)
import Effect (Effect)
import Screeps (resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, setAllMemory)
import Screeps.Types (Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type WallRepairerMemory = { role :: Role, working :: Boolean }
type WallRepairer = { creep :: Creep, mem :: WallRepairerMemory }

setMemory :: WallRepairer -> WallRepairerMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runWallRepairer :: WallRepairer -> Effect Unit
runWallRepairer repairer@{ creep, mem } = do

  if mem.working
  then do
    case ((amtCarrying creep resource_energy) == 0) of
      true -> 
        do
          setMemory repairer (mem { working = false })
      false ->
        repairStructure creep true
  else do
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> do
        setMemory repairer (mem { working = true })
      false -> do
        collectEnergy creep true
              

