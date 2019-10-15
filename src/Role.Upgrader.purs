module Role.Upgrader (runUpgrader, UpgraderMemory, Upgrader) where

import Prelude

import CreepRoles (Role)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_sources_active, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, setAllMemory, upgradeController)
import Screeps.Game (getGameGlobal)
import Screeps.Room (controller)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByPath)
import Screeps.Types (Creep, FindContext(..), TargetPosition(..))

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
      setMemory upgrader (mem { working = true }) 
    else do
      source <- findClosestByPath (pos creep) (OfType find_sources_active)
      case source of
        Right (Just targetSource) -> do
          harvestResult <- harvestSource creep targetSource
          if harvestResult == err_not_in_range
          then moveTo creep (TargetObj targetSource) # ignoreM
          else pure unit
        _ -> pure unit
      
