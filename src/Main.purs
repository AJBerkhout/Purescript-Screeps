module Main (loop) where

import Prelude

import CreepClassification (CreepMemory(..), UnknownCreepType(..), VocationalCreep(..), classifyCreep, spawnCreep)
import CreepRoles (Role(..))
import Data.Array (fromFoldable, length, mapMaybe)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Role.Builder (runBuilder)
import Role.Harvester (runHarvester)
import Role.Upgrader (runUpgrader)
import Screeps.Constants (ok, part_carry, part_move, part_work)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Spawn (canCreateCreep)
import Screeps.Types (Creep, Spawn)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

noName :: Maybe String 
noName = Nothing

matchUnit :: Either UnknownCreepType VocationalCreep -> Effect Unit
matchUnit (Right (Harvester creep)) = runHarvester creep
matchUnit (Right (Upgrader creep)) = runUpgrader creep
matchUnit (Right (Builder creep)) = runBuilder creep
matchUnit (Left (UnknownCreepType err)) = log $ "One of the creeps has a memory I can't parse.\n" <> err

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = classifyCreep creep >>= matchUnit

spawnNewCreeps :: Spawn -> Effect Unit
spawnNewCreeps spawn =
  let 
    minHarvesters = 2
    minBuilder = 2
  in 
    do
      thisGame <- getGameGlobal
      creepsAndRolesObj <- for (creeps thisGame) $ classifyCreep 
      let
        creepsAndRoles = fromFoldable creepsAndRolesObj 
        harvesters = creepsAndRoles # mapMaybe (case _ of 
          (Right (Harvester h)) -> Just h
          _ -> Nothing)
        upgraders = creepsAndRoles # mapMaybe (case _ of 
          (Right (Upgrader u)) -> Just u
          _ -> Nothing)
        builders = creepsAndRoles # mapMaybe (case _ of 
          (Right (Builder b)) -> Just b
          _ -> Nothing)

      if ((length harvesters) < minHarvesters) then
        spawnCreep spawn 
          [part_move, part_work, part_work, part_carry] noName 
          (HarvesterMemory {role: HarvesterRole})
        >>= logShow
      else if ((length builders) < minBuilder) then
        spawnCreep spawn 
          [part_move, part_work, part_work, part_carry] noName 
          (BuilderMemory {role: BuilderRole, working: true})
        >>= logShow
      else 
        spawnCreep spawn 
          [part_move, part_work, part_work, part_carry] noName 
          (UpgraderMemory {role: UpgraderRole, working: true})
        >>= logShow
            

loop :: Effect Unit
loop = do
  game <- getGameGlobal

  for_ (spawns game) \spawn -> do
    if canCreateCreep spawn [part_move, part_work, part_work, part_carry] == ok
    then spawnNewCreeps spawn
    else pure unit

  for_ (creeps game) \n -> do
    runCreepRole n
    

