module Screeps.Defense where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (find_hostile_creeps, find_my_structures)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (findClosestByRange, findClosestByRange')
import Screeps.Structure (hits, hitsMax)
import Screeps.Tower (attack, repair)
import Screeps.Types (FindContext(..), OwnedStructure, RawOwnedStructure, RawRoomObject, RawStructure, RawTower, Structure, Tower)

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
  closestDamageStructure <- findClosestByRange' roomPos (OfType find_my_structures) damaged
  case closestDamageStructure of
    Left e -> pure unit
    Right maybeStruct -> 
      case maybeStruct of
        Just struct -> do
          code <- repair tower struct
          pure unit
        Nothing -> pure unit
  closestHostile <- findClosestByRange roomPos (OfType find_hostile_creeps)
  case closestHostile of 
    Left e -> pure unit
    Right maybeEnemy ->
      case maybeEnemy of
        Just enemy -> do
          code <- attack tower enemy
          pure unit
        Nothing -> 
          pure unit
