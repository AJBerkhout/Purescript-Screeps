module CommonActions
  ( upgrade
  , collectEnergy
  , buildStructure
  , transport
  , repairStructure
  ) where

import Prelude

import Data.Array (head)
import Data.Foldable (any)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect (Effect)
import Screeps (err_not_in_range, find_my_construction_sites, find_my_spawns, find_my_structures, find_ruins, find_sources_active, find_structures, part_work, resource_energy)
import Screeps.Container (storeCapacity, storeGet, toContainer)
import Screeps.Creep (body, build, harvestSource, moveTo, repair, transferToStructure, upgradeController, withdraw)
import Screeps.Extension (toExtension)
import Screeps.Extension as Extension
import Screeps.Rampart (toRampart)
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath')
import Screeps.Ruin (energy)
import Screeps.Spawn (toSpawn)
import Screeps.Spawn as Spawn
import Screeps.Storage (toStorage)
import Screeps.Storage as Storage
import Screeps.Structure (hits, hitsMax)
import Screeps.Tower (toTower)
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), Structure, TargetPosition(..))
import Screeps.Wall (toWall)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore

upgrade :: Creep -> Effect Unit
upgrade creep =
  case (controller (room creep)) of
    Just controller -> do
      upgradeResult <- upgradeController creep controller
      if upgradeResult == err_not_in_range
      then moveTo creep (TargetObj controller) # ignoreM
      else pure unit
    Nothing -> pure unit
 
isNonEmptyContainer :: Structure Unit -> Boolean
isNonEmptyContainer struct =
  case toStorage struct of
    Just storage ->
      Storage.storeGet storage resource_energy > 0
    Nothing ->
      case toContainer struct of
        Just container ->
          storeGet container resource_energy > storeCapacity container / 2
        Nothing ->
          false

collectEnergy :: Creep -> Boolean -> Effect Unit
collectEnergy creep useContainers = do
  closestRuin <- findClosestByPath' (pos creep) (OfType find_ruins) (closestPathOpts { filter = Just (\n -> energy n > 0)})
  
  case closestRuin of
    Just ruin -> do
      harvestResult <- withdraw creep ruin resource_energy
      if harvestResult == err_not_in_range
      then creep `moveTo` (TargetObj ruin) # ignoreM
      else pure unit
    Nothing -> do
      closestSource <- findClosestByPath' (pos creep) (OfType find_sources_active) (closestPathOpts)
      case closestSource of
        Just targetSource -> do
          harvestResult <- harvestSource creep targetSource
          if harvestResult == err_not_in_range
          then moveTo creep (TargetObj targetSource) # ignoreM
          else pure unit
        Nothing -> do
          if useContainers then do
            closestContainer <- findClosestByPath' (pos creep) (OfType find_structures) (closestPathOpts {filter = Just isNonEmptyContainer})
            case closestContainer of
              Just (c :: Structure Unit) -> do
                r <- withdraw creep c resource_energy
                if r == err_not_in_range
                then moveTo creep (TargetObj c) # ignoreM
                else pure unit
              Nothing -> do pure unit
          else do 
            pure unit

desiredTarget :: (forall a. Structure a) -> Boolean
desiredTarget struct
  | Just spawn <- toSpawn struct = Spawn.energy spawn < Spawn.energyCapacity spawn
  | Just extension <- toExtension struct = Extension.energy extension < Extension.energyCapacity extension
  | Just tower <- toTower struct = Tower.energy tower < Tower.energyCapacity tower
  | Just storage <- toStorage struct = Storage.storeGet storage resource_energy < Storage.storeCapacity storage
  | otherwise = false 

transport :: Creep -> Effect Unit
transport creep = do
  closestStructure <- findClosestByPath' (pos creep) (OfType find_my_structures) (closestPathOpts { filter = Just desiredTarget })
  case closestStructure of
    Just (energyStructure :: forall a. Structure a) -> do
      transferResult <- transferToStructure creep energyStructure resource_energy
      if transferResult == err_not_in_range
      then moveTo creep (TargetObj energyStructure) # ignoreM
      else pure unit
    Nothing -> 
      let parts = any (\n -> n.type == part_work) (body creep) in
        if parts
        then buildStructure creep 
        else
          case (head (find (room creep) find_my_spawns)) of
            Just spawn -> moveTo creep (TargetObj spawn) # ignoreM
            Nothing -> pure unit

buildStructure :: Creep -> Effect Unit
buildStructure creep = do
  struct <- findClosestByPath (pos creep) (OfType find_my_construction_sites)
  case struct of
    Nothing -> upgrade creep
    Just targetSite -> do
      buildResult <- build creep targetSite
      if buildResult == err_not_in_range 
      then moveTo creep (TargetObj targetSite) # ignoreM
      else pure unit

isDamagedAndIsTarget :: Boolean -> (forall a. Structure a) -> Boolean
isDamagedAndIsTarget repairWalls struct = 
  if (repairWalls) then
    if (isJust (toWall struct) && (toNumber (hits struct) / toNumber (hitsMax struct)) < 0.0003) then
      true
    else
      if (isJust (toRampart struct) && (toNumber (hits struct) / toNumber (hitsMax struct)) < 0.0003) then
        true
      else  
        hits struct < (hitsMax struct / 2)
  else 
    (isNothing (toWall struct)) && (isNothing (toRampart struct)) && hits struct < (hitsMax struct / 2)

repairStructure :: Creep -> Boolean -> Effect Unit
repairStructure creep repairWalls = do
  struct <- findClosestByPath' (pos creep) (OfType find_my_structures) (closestPathOpts {filter = Just (isDamagedAndIsTarget repairWalls)}) 
  case struct of
    Nothing -> 
      buildStructure creep
    Just (damagedBuilding :: forall a. Structure a) -> do
      repairResult <- repair creep damagedBuilding
      if repairResult == err_not_in_range then
        moveTo creep (TargetObj damagedBuilding) # ignoreM
      else do 
        pure unit