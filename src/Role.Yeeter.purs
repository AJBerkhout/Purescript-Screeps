module Role.Yeeter (runYeeter, YeeterMemory, Yeeter) where

import Prelude

import CreepRoles (Role)
import Data.Array (filter, head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (container_capacity, err_not_in_range, find_dropped_energy, find_dropped_resources, find_my_structures, resource_energy)
import Screeps.Container (storeGet, toContainer)
import Screeps.Creep (amtCarrying, carryCapacity, moveTo, pickup, setAllMemory, transferToStructure, withdraw)
import Screeps.Extension as Extension
import Screeps.Room (controller, find, find')
import Screeps.RoomObject (room)
import Screeps.Spawn as Spawn
import Screeps.Types (Creep, RawRoomObject, RawStructure, Structure, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type YeeterMemory = { role :: Role, working :: Boolean }
type Yeeter = { creep :: Creep, mem :: YeeterMemory }

setMemory :: Yeeter -> YeeterMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

isNonEmptyContainer :: forall a. Structure a -> Boolean
isNonEmptyContainer struct =
  case toContainer struct of
    Just container ->
      storeGet container resource_energy >= (container_capacity / 3)
    Nothing ->
      false

desiredTarget :: forall a. RawRoomObject (RawStructure a) -> Boolean
desiredTarget struct = 
  case (Spawn.toSpawn struct) of
    Just spawn -> 
      Spawn.energy spawn < Spawn.energyCapacity spawn
    Nothing ->
      case (Extension.toExtension struct) of
        Just ext ->
          Extension.energy ext < Extension.energyCapacity ext
        Nothing -> false

runYeeter :: Yeeter -> Effect Unit
runYeeter yeeter@{ creep, mem } =
  case mem.working of
    true ->
      case amtCarrying creep resource_energy == carryCapacity creep of
        false ->
          let
            closestNonEmptyContainer = head (find' (room creep) find_my_structures isNonEmptyContainer)
          in
            case head (find (room creep) find_dropped_resources) of
              Just e -> do
                code <- pickup creep e
                if code == err_not_in_range then
                  moveTo creep (TargetObj e) # ignoreM
                else
                  pure unit
              Nothing -> 
                case closestNonEmptyContainer of
                  Just container -> do
                    code <- withdraw creep container resource_energy
                    if code == err_not_in_range then
                      moveTo creep (TargetObj container) # ignoreM
                    else 
                      pure unit
                  Nothing -> 
                    pure unit
        true ->
          setMemory yeeter (mem {working = false})  
    false ->
      case amtCarrying creep resource_energy == 0 of
        true ->
          setMemory yeeter (mem {working = true})
        false -> 
          case (head (filter desiredTarget (find (room creep) find_my_structures))) of
            Just struct -> do
              code <- transferToStructure creep struct resource_energy
              if code == err_not_in_range then
                moveTo creep (TargetObj struct) # ignoreM
              else 
                pure unit
            Nothing -> 
              case controller (room creep) of
                Just con -> do
                  code <- transferToStructure creep con resource_energy
                  if code == err_not_in_range then
                    moveTo creep (TargetObj con) # ignoreM
                  else
                    pure unit
                Nothing -> 
                  pure unit
                
                    
