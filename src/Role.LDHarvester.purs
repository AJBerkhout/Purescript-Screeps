module Role.LDHarvester (runLDHarvester) where

import Prelude

import CreepRoles (LDHarvester)
import Data.Array (head, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Screeps (err_not_in_range, find_my_structures, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, harvestSource, moveTo, transferToStructure)
import Screeps.Extension as Extension
import Screeps.Room (RoomIdentifier(..), find, findExitTo, name)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByRange)
import Screeps.Spawn as Spawn
import Screeps.Tower as Tower
import Screeps.Types (FindContext(..), RawRoomObject, RawStructure, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

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
runLDHarvester { creep, mem: {targetRoom, home} } =
  let 
    atCapacity = amtCarrying creep resource_energy == carryCapacity creep
  in
    case atCapacity of
      true -> 
        case (name (room creep) == home) of
          true ->
            case (head (filter desiredTarget (find (room creep) find_my_structures))) of
              Nothing -> pure unit
              Just spawn1 -> do
                transferResult <- transferToStructure creep spawn1 resource_energy
                if transferResult == err_not_in_range
                then moveTo creep (TargetObj spawn1) # ignoreM
                else pure unit
          false ->
            let maybeExit = findExitTo (room creep) (RoomName home) in
            case maybeExit of
              Left e -> 
                do pure unit
              Right exitSearch -> do
                exit <- findClosestByRange (pos creep) (OfType exitSearch)
                case exit of
                  Right (Just position) -> do
                    ignoreM (moveTo creep (TargetPos position))
                  _-> do pure unit
      false -> 
        case (targetRoom == (name (room creep))) of
          true->
            case head (find (room creep) find_sources) of
              Nothing -> pure unit
              Just targetSource -> do
                harvestResult <- harvestSource creep targetSource
                if harvestResult == err_not_in_range
                then moveTo creep (TargetObj targetSource) # ignoreM
                else pure unit
          false -> 
            let maybeExit = findExitTo (room creep) (RoomName targetRoom) in
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
                
