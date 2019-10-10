module Role.Harvester (runHarvester) where

import Prelude

import CreepRoles (Harvester)
import Data.Array (head, filter)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_my_structures, find_sources, resource_energy, structure_spawn, structure_extension)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure)
import Screeps.Game (getGameGlobal)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Structure (structureType)
import Screeps.Types (RawRoomObject, RawStructure, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

desiredTarget :: forall a. RawRoomObject (RawStructure a) -> Boolean
desiredTarget struct = 
  (structureType struct) == structure_spawn || (structureType struct) == structure_extension

runHarvester :: Harvester -> Effect Unit
runHarvester { creep, mem } =

  if amtCarrying creep resource_energy < carryCapacity creep
  then
    case head (find (room creep) find_sources) of
      Nothing -> pure unit
      Just targetSource -> do
        harvestResult <- harvestSource creep targetSource
        if harvestResult == err_not_in_range
        then moveTo creep (TargetObj targetSource) # ignoreM
        else pure unit
        
  else do
    game <- getGameGlobal
    case (head (filter desiredTarget (find (room creep) find_my_structures))) of
      Nothing -> pure unit
      Just spawn1 -> do
        transferResult <- transferToStructure creep spawn1 resource_energy
        if transferResult == err_not_in_range
        then moveTo creep (TargetObj spawn1) # ignoreM
        else pure unit
