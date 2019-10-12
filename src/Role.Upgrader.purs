module Role.Upgrader (runUpgrader, UpgraderMemory, Upgrader) where

import Prelude

import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, say, setAllMemory, upgradeController)
import Screeps.Game (getGameGlobal)
import Screeps.Room (find, controller)
import Screeps.RoomObject (room)
import Screeps.Types (TargetPosition(..), Creep)

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
      s <- say creep "Harvesting"
      setMemory upgrader (mem { working = false })
    else do
      game <- getGameGlobal
      case (controller (room creep)) of
        Nothing -> pure unit
        Just controller -> do
          upgradeResult <- upgradeController creep controller
          if upgradeResult == err_not_in_range
          then moveTo creep (TargetObj controller) # ignoreM
          else pure unit

  else 
    if amtCarrying creep resource_energy == carryCapacity creep
    then do
      s <- say creep "Upgrading"
      setMemory upgrader (mem { working = true }) 
    else
      case head (find (room creep) find_sources) of
        Nothing -> pure unit
        Just targetSource -> do
          harvestResult <- harvestSource creep targetSource
          if harvestResult == err_not_in_range
          then moveTo creep (TargetObj targetSource) # ignoreM
          else pure unit
        
