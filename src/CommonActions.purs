module CommonActions
  ( upgrade
  , collectEnergy
  , buildStructure
  , transport) where

import Prelude

import CreepSpawning (ignoreM)
import Data.Array (head)
import Data.Distributive (collect)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_my_structures, find_sources_active, resource_energy)
import Screeps.Creep (build, harvestSource, moveTo, say, transferToStructure, upgradeController, withdraw)
import Screeps.Extension (toExtension)
import Screeps.Extension as Extension
import Screeps.Room (controller, find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, findClosestByPath')
import Screeps.Spawn (toSpawn)
import Screeps.Spawn as Spawn
import Screeps.Tower (toTower)
import Screeps.Tower as Tower
import Screeps.Types (Creep, FindContext(..), Structure, TargetPosition(..))


upgrade :: Creep -> Effect Unit
upgrade creep =
  case (controller (room creep)) of
    Just controller -> do
      upgradeResult <- upgradeController creep controller
      if upgradeResult == err_not_in_range
      then moveTo creep (TargetObj controller) # ignoreM
      else pure unit
    Nothing -> pure unit
  
collectEnergy :: Creep -> Boolean -> Effect Unit
collectEnergy creep useContainers = do
  closestSource <- findClosestByPath (pos creep) (OfType find_sources_active) 
  case closestSource of
    Nothing -> pure unit
    Just targetSource -> do
      harvestResult <- harvestSource creep targetSource
      if harvestResult == err_not_in_range
      then creep `moveTo` (TargetObj targetSource) # ignoreM
      else pure unit

desiredTarget :: (forall a. Structure a) -> Boolean
desiredTarget struct
  | Just spawn <- toSpawn struct = Spawn.energy spawn < Spawn.energyCapacity spawn
  | Just extension <- toExtension struct = Extension.energy extension < Extension.energyCapacity extension
  | Just tower <- toTower struct = Tower.energy tower < Tower.energyCapacity tower
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
    Nothing -> buildStructure creep 

buildStructure :: Creep -> Effect Unit
buildStructure creep = 
  case head (find (room creep) find_construction_sites) of
    Nothing -> buildStructure creep
    Just targetSite -> do
      buildResult <- creep `build` targetSite
      if buildResult == err_not_in_range 
      then creep `moveTo` (TargetObj targetSite) # ignoreM
      else pure unit