module Role.Repairer (runRepairer, RepairerMemory, Repairer) where

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

type RepairerMemory = { role :: Role, working :: Boolean }
type Repairer = { creep :: Creep, mem :: RepairerMemory }

setMemory :: Repairer -> RepairerMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runRepairer :: Repairer -> Effect Unit
runRepairer repairer@{ creep, mem } = do

  if mem.working
  then do
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> 
        do
          setMemory repairer (mem { working = false })
      false ->
        collectEnergy creep true
  else do
    case ((amtCarrying creep resource_energy) == 0) of
      true -> do
        setMemory repairer (mem { working = true })
      false -> do
        repairStructure creep false
      
        
              

