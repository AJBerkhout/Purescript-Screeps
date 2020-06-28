module Role.Harvester (runHarvester, HarvesterMemory, Harvester) where

import Prelude

import CommonActions (collectEnergy, transport)
import CreepRoles (Role)
import Effect (Effect)
import Screeps (resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, setAllMemory)
import Screeps.Types (Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type HarvesterMemory = { role :: Role, working :: Boolean }
type Harvester = { creep :: Creep, mem :: HarvesterMemory }

setMemory :: Harvester -> HarvesterMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runHarvester :: Harvester -> Effect Unit
runHarvester harvester@{creep, mem} =
  if mem.working
  then
    if amtCarrying creep resource_energy == 0
    then do
      setMemory harvester (mem { working = false })
    else do
      transport creep
  else 
    if amtCarrying creep resource_energy == carryCapacity creep
    then do
      setMemory harvester (mem { working = true }) 
    else do
      collectEnergy creep true
  