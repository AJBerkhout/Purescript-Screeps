module Role.Upgrader (runUpgrader, UpgraderMemory, Upgrader) where

import Prelude

import CommonActions (collectEnergy, upgrade)
import CreepRoles (Role)
import Effect (Effect)
import Effect.Console (log)
import Screeps (resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, setAllMemory)
import Screeps.Types (Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 


type UpgraderMemory = { role :: Role, working :: Boolean }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }

setMemory :: Upgrader -> UpgraderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runUpgrader :: Upgrader -> Effect Unit
runUpgrader upgrader@{ creep, mem } =

  if mem.working
  then
    if amtCarrying creep resource_energy == 0
    then do
      setMemory upgrader (mem { working = false })
    else do
      upgrade creep

  else 
    if amtCarrying creep resource_energy == carryCapacity creep
    then do
      setMemory upgrader (mem { working = true }) 
    else do
      collectEnergy creep true
      
