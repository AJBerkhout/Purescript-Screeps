module Role.Guard 
  ( runGuard
  , GuardMemory
  , Guard
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_hostile_creeps, part_attack, part_move, part_tough)
import Screeps.Creep (attackCreep, moveTo, setAllMemory)
import Screeps.RoomObject (pos)
import Screeps.RoomPosition (findClosestByPath)
import Screeps.Types (BodyPartType, Creep, FindContext(..), TargetPosition(..))
import Util (ignoreM)

constructionPlans :: Array (Array BodyPartType)
constructionPlans =
  [ [ part_tough, part_tough, part_tough, part_tough, part_tough, part_tough, part_move, part_move, part_move, part_move, part_move, part_move, part_attack, part_attack, part_attack, part_attack, part_attack, part_attack ]
  , [ part_tough, part_tough, part_tough, part_tough, part_tough, part_move, part_move, part_move, part_move, part_move, part_attack, part_attack, part_attack, part_attack, part_attack ] -- 700
  , [ part_tough, part_tough, part_tough, part_tough, part_tough, part_move, part_move, part_move, part_move, part_attack, part_attack, part_attack, part_attack, part_attack ] -- 650
  , [ part_tough, part_tough, part_tough, part_tough, part_tough, part_tough, part_move, part_move, part_move, part_move, part_move, part_attack, part_attack, part_attack ] -- 550 (max room 2)
  , [ part_tough, part_tough, part_tough, part_tough, part_move, part_move, part_move, part_attack, part_attack ] -- 350
  , [ part_tough, part_tough, part_tough, part_tough, part_move, part_move, part_attack, part_attack ] -- 300 (just the spawn)
  ]

type GuardMemory = { role :: Role }
type Guard = { creep :: Creep, mem :: GuardMemory }

setMemory :: Guard -> GuardMemory -> Effect Unit
setMemory { creep } mem = setAllMemory creep mem

runGuard :: Guard -> Effect Unit
runGuard guard@{ creep, mem } = do
  maybeHostile <- findClosestByPath (pos creep) (OfType find_hostile_creeps) 
  case maybeHostile of
    Nothing -> pure unit
    Just hostile -> do
      result <- creep `attackCreep` hostile
      if result == err_not_in_range 
      then creep `moveTo` (TargetObj hostile) # ignoreM
      else pure unit
