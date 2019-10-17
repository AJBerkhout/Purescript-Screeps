module Role.Harvester (runHarvester, HarvesterMemory, Harvester) where

import Prelude

import CreepRoles (Role)
import Data.Array (head, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Screeps (err_not_in_range, find_my_structures, find_sources_active, resource_energy)
import Screeps.Container (toContainer)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, setAllMemory, transferToStructure, upgradeController)
import Screeps.Extension as Extension
import Screeps.Game (getGameGlobal)
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByPath)
import Screeps.Spawn as Spawn
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), RawRoomObject, RawStructure, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type HarvesterMemory = { role :: Role, working :: Boolean }
type Harvester = { creep :: Creep, mem :: HarvesterMemory }

setMemory :: Harvester -> HarvesterMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

desiredTarget :: forall a. RawRoomObject (RawStructure a) -> Boolean
desiredTarget struct = 
  case (Tower.toTower struct) of
    Just tower -> 
       Tower.energy tower < Tower.energyCapacity tower
    Nothing ->
      case (Spawn.toSpawn struct) of
        Just spawn -> 
          Spawn.energy spawn < Spawn.energyCapacity spawn
        Nothing ->
          case (Extension.toExtension struct) of
            Just ext ->
              Extension.energy ext < Extension.energyCapacity ext
            Nothing -> false

runHarvester :: Harvester -> Effect Unit
runHarvester harvester@{creep, mem} =
  if mem.working
  then
    if amtCarrying creep resource_energy == 0
    then do
      setMemory harvester (mem { working = false })
    else do
      game <- getGameGlobal
      case (head (filter desiredTarget (find (room creep) find_my_structures))) of
        Nothing -> 
          case (head (filter (\n -> isJust (toContainer n)) (find (room creep) find_my_structures))) of
          Just container -> do
            transferResult <- transferToStructure creep container resource_energy
            if transferResult == err_not_in_range
            then moveTo creep (TargetObj container) # ignoreM
            else pure unit
          Nothing ->
            case (controller (room creep)) of
              Nothing -> pure unit
              Just controller -> do
                upgradeResult <- upgradeController creep controller
                if upgradeResult == err_not_in_range
                then moveTo creep (TargetObj controller) # ignoreM
                else pure unit
        Just spawn1 -> do
          transferResult <- transferToStructure creep spawn1 resource_energy
          if transferResult == err_not_in_range
          then moveTo creep (TargetObj spawn1) # ignoreM
          else pure unit
  else
    if amtCarrying creep resource_energy == carryCapacity creep
    then do
      setMemory harvester (mem { working = true }) 
    else do
      source <- findClosestByPath (pos creep) (OfType find_sources_active)
      case source of
        Right (Just targetSource) -> do
          harvestResult <- harvestSource creep targetSource
          if harvestResult == err_not_in_range
          then moveTo creep (TargetObj targetSource) # ignoreM
          else pure unit
        _ -> pure unit 