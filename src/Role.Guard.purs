module Role.Guard (runGuard, GuardMemory, Guard) where

import Prelude
import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_hostile_creeps, find_my_spawns)
import Screeps.Creep (attackCreep, moveTo, setAllMemory)
import Screeps.Room (find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByRange)
import Screeps.Types (Creep, FindContext(..), TargetPosition(..))

type GuardMemory = { role :: Role}
type Guard = { creep :: Creep, mem :: GuardMemory }

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

setMemory :: Guard -> GuardMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runGuard :: Guard -> Effect Unit
runGuard guard@{creep, mem} =
  let 
    maybeSpawn = 
      head (find (room creep) find_my_spawns)
  in
    case maybeSpawn of
      Just spawn -> do
        closestHostile <- findClosestByRange (pos spawn) (OfType find_hostile_creeps)
        case closestHostile of
          Just enemy -> do
            attackResult <- attackCreep creep enemy
            if attackResult == err_not_in_range then
              moveTo creep (TargetObj enemy) # ignoreM
            else
              pure unit
          _ -> 
            moveTo creep (TargetObj spawn) # ignoreM
      Nothing -> do
        closestHostile <- findClosestByRange (pos creep) (OfType find_hostile_creeps)
        case closestHostile of
          Just enemy -> do
            attackResult <- attackCreep creep enemy
            if attackResult == err_not_in_range then
              moveTo creep (TargetObj enemy) # ignoreM
            else
              pure unit
          _ -> pure unit