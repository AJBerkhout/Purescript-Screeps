module Screeps.Defense where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (find_hostile_creeps)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (findClosestByRange)
import Screeps.Structure (hits, hitsMax)
import Screeps.Tower (attack)
import Screeps.Types (FindContext(..), RawOwnedStructure, RawRoomObject, RawStructure, RawTower, Structure)

damaged :: forall a. Structure a -> Boolean
damaged struct = 
  let
    currentHealth = hits struct
    maxHealth = hitsMax struct
  in
    currentHealth < maxHealth

runTower :: RawRoomObject (RawStructure (RawOwnedStructure (RawTower))) -> Effect Unit
runTower tower = do
  let roomPos = pos tower 
  closestHostile <- findClosestByRange roomPos (OfType find_hostile_creeps)
  case closestHostile of 
    Nothing -> pure unit
    Just enemy -> do
      code <- attack tower enemy
      pure unit
