module CreepSpawning where

import Prelude

import CreepRoles (CreepMemory(..), Role(..), VocationalCreep(..), classifyCreep, spawnCreep)
import Data.Argonaut (stringify)
import Data.Array (concat, (..), fromFoldable, length, mapMaybe)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Screeps.Constants (part_carry, part_move, part_work)
import Screeps.Game (creeps, getGameGlobal)
import Screeps.Room (energyAvailable, energyCapacityAvailable)
import Screeps.RoomObject (room)
import Screeps.Spawn (energy)
import Screeps.Types (ReturnCode, Spawn)

spawnCreepIfNeeded :: Spawn -> Effect Unit
spawnCreepIfNeeded spawn =
  let 
    minHarvesters = 2
    minUpgraders = 1
    minBuilders = 3
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
        maxEnergy = (energyCapacityAvailable (room spawn))
      if ((length harvesters) < minHarvesters && maxEnergy >= energyAvailable (room spawn)) then
        createBalancedCreep spawn maxEnergy (HarvesterMemory {role: HarvesterRole})
        >>= logShow
      else if ((length builders) < minBuilders && maxEnergy >= energyAvailable (room spawn)) then
        createBalancedCreep spawn maxEnergy (BuilderMemory {role: BuilderRole, working: true})
        >>= logShow
      else if ((length upgraders) < minUpgraders && maxEnergy >= energyAvailable (room spawn)) then
        createBalancedCreep spawn maxEnergy (UpgraderMemory {role: UpgraderRole, working: true})
        >>= logShow
      else 
        pure unit

createBalancedCreep :: Spawn -> Int -> CreepMemory -> Effect (Either ReturnCode String)
createBalancedCreep spawn energy mem =
  let 
    numberOfParts = energy / 200
    moveParts = map (\n -> part_move) (0..(numberOfParts-1))
    workParts = map (\n -> part_carry) (0..(numberOfParts-1))
    carryParts = map (\n -> part_work) (0..(numberOfParts-1))
    allParts = concat [moveParts, workParts, carryParts]
    noName = Nothing
  in do
    spawnCreep spawn allParts noName mem