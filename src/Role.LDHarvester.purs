module Role.LDHarvester (runLDHarvester, LDHarvesterMemory, LDHarvester) where

import Prelude

import CreepRoles (Role)
import Data.Array (head, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_my_structures, find_sources, find_sources_active, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, setAllMemory, transferToStructure)
import Screeps.Extension as Extension
import Screeps.Room (RoomIdentifier(..), controller, find, findExitTo, name)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByPath, findClosestByRange)
import Screeps.Spawn as Spawn
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), RawRoomObject, RawStructure, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type LDHarvesterMemory = { role :: Role, working :: Boolean, targetRoom :: String, home :: String }
type LDHarvester = { creep :: Creep, mem :: LDHarvesterMemory }

setMemory :: LDHarvester -> LDHarvesterMemory -> Effect Unit
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
            
runLDHarvester :: LDHarvester -> Effect Unit
runLDHarvester ld@{ creep, mem } =
  if mem.working then
    case (amtCarrying creep resource_energy == 0) of
      false -> 
        case (name (room creep) == mem.home) of
          true ->
            case (head (filter desiredTarget (find (room creep) find_my_structures))) of
              Nothing -> 
                case (controller (room creep)) of
                  Just c -> do
                    transferResult <- transferToStructure creep c resource_energy
                    if transferResult == err_not_in_range
                    then moveTo creep (TargetObj c) # ignoreM
                    else pure unit
                  Nothing ->
                    pure unit
              Just spawn1 -> do
                transferResult <- transferToStructure creep spawn1 resource_energy
                if transferResult == err_not_in_range
                then moveTo creep (TargetObj spawn1) # ignoreM
                else pure unit
          false ->
            let maybeExit = findExitTo (room creep) (RoomName mem.home) in
            case maybeExit of
              Left e -> 
                do pure unit
              Right exitSearch -> do
                exit <- findClosestByRange (pos creep) (OfType exitSearch)
                case exit of
                  Right (Just position) -> do
                    ignoreM (moveTo creep (TargetPos position))
                  _-> do pure unit
      true -> 
        setMemory ld (mem {working = false})
  else 
    case amtCarrying creep resource_energy == carryCapacity creep of
      true -> setMemory ld (mem {working = true})
      false ->
        case (mem.targetRoom == (name (room creep))) of
          true -> do
            source <- findClosestByPath (pos creep) (OfType find_sources_active)
            case source of
              Right (Just targetSource) -> do
                harvestResult <- harvestSource creep targetSource
                if harvestResult == err_not_in_range
                then moveTo creep (TargetObj targetSource) # ignoreM
                else pure unit
              _ -> pure unit
          false -> 
            let maybeExit = findExitTo (room creep) (RoomName mem.targetRoom) in
              case maybeExit of
                Left e -> do
                  do pure unit
                Right exitSearch -> do
                  exit <- findClosestByRange (pos creep) (OfType exitSearch)
                  case exit of
                    Right (Just position) -> do
                      ignoreM (moveTo creep (TargetPos position))
                    _ -> do 
                      pure unit
                  