module Main (loop) where

import Prelude

import CreepClassification (CreepMemory(..), UnknownCreepType(..), VocationalCreep(..), classifyCreep, spawnCreep)
import CreepRoles (Role(..))
import Data.Array (catMaybes, filter, fromFoldable, length, mapMaybe, zip)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (for_, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Role.Builder (runBuilder)
import Role.Builder as Builder
import Role.Guard (runGuard)
import Role.Guard as Guard
import Role.Harvester (runHarvester)
import Role.Harvester as Harvester
import Role.Tower (runTower)
import Role.Upgrader (runUpgrader)
import Role.Upgrader as Upgrader
import Screeps (find_hostile_creeps, find_my_structures, ok, part_move)
import Screeps.Controller (level)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Room (controller, energyAvailable, energyCapacityAvailable, find)
import Screeps.RoomObject (room)
import Screeps.Tower (toTower)
import Screeps.Types (BodyPartType, Creep, Spawn, Structure)
import Util (bodyPartCost, (<<#>>))

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
matchUnit (Right (Guard creep)) = runGuard creep
matchUnit (Left (UnknownCreepType err)) = log $ "One of the creeps has a memory I can't parse.\n" <> err

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = classifyCreep creep >>= matchUnit

shouldSpawnCreep :: { nCreeps :: Int, totalCapacity :: Int } -> Boolean
shouldSpawnCreep { nCreeps, totalCapacity } = 
  nCreeps < 8
    

constructionPlan :: Array (Array BodyPartType) -> Int -> Array BodyPartType
constructionPlan plans budget =
  let 
    costs = plans <<#>> bodyPartCost <#> sum
    maybePlan =
      zip plans costs 
      # Array.find (\(Tuple plan cost) -> cost <= budget)
      <#> (\(Tuple plan _cost) -> plan) 
  in fromMaybe [] maybePlan


spawnNewCreeps :: Spawn -> Int -> Int -> Boolean -> Effect Unit
spawnNewCreeps spawn budget controllerLevel anyHostiles = do
  game <- getGameGlobal
  creepsAndRolesObj <- for (creeps game) $ classifyCreep 

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

    minHarvesters = 2
    minBuilders = 2
    minUpgraders = 1

  if anyHostiles then spawnGuard
  else if (length harvesters) < minHarvesters then spawnHarvester
  else if (length builders) < minBuilders then spawnBuilder
  else if (length upgraders) < minUpgraders then spawnUpgrader
  else spawnHarvester

  where
    spawnGuard = do
      let plan = constructionPlan Guard.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: GuardMemory {role: GuardRole}, dryRun: false }
      if returnCode == ok then log $ "Spawned Guard " <> show plan
      else log $ "couldn't create guard with plan: " <> show plan <> ". Error code: " <> show returnCode

    spawnHarvester = do
      let plan = constructionPlan Harvester.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: HarvesterMemory {role: HarvesterRole, job: Harvester.Harvesting}, dryRun: false }
      if returnCode == ok then log $ "Spawned Harvester " <> show plan
      else log $ "couldn't create harvester with plan: " <> show plan <> ". Error code: " <> show returnCode

    spawnUpgrader = do
      let plan = constructionPlan Upgrader.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: UpgraderMemory {role: UpgraderRole, job: Upgrader.Harvesting}, dryRun: false }
      if returnCode == ok then log $ "Spawned Upgrader " <> show plan
      else log $ "couldn't create upgrader with plan: " <> show plan <> ". Error code: " <> show returnCode

    spawnBuilder = do
      let plan = constructionPlan Builder.constructionPlans budget
      returnCode <- spawnCreep spawn plan noName { memory: BuilderMemory {role: BuilderRole, job: Builder.Harvesting}, dryRun: false }
      if returnCode == ok then log $ "Spawned Builder " <> show plan
      else log $ "couldn't create builder with plan: " <> show plan <> ". Error code: " <> show returnCode
                

loop :: Effect Unit
loop = do
  game <- getGameGlobal
  creepMemories <- for (creeps game # fromFoldable) classifyCreep
  let 
    nCreeps = 
      creepMemories <#> hush # catMaybes 
      # filter (case _ of 
        Harvester _ -> true
        Builder _ -> true
        Upgrader _ -> true
        _ -> false)
      # length
  
  
  for_ (spawns game) \spawn -> do
    dryRunReturnCode <- spawnCreep spawn [part_move] noName { memory: GuardMemory { role: GuardRole }, dryRun: true }
    
    let 
      canSpawn = dryRunReturnCode == ok
      anyHostiles = find (room spawn) find_hostile_creeps # length # (_ > 0)
      totalCapacity = energyCapacityAvailable (room spawn)
      shouldSpawn = shouldSpawnCreep { nCreeps, totalCapacity } || anyHostiles
      controllerLevel = controller (room spawn) <#> level # fromMaybe 0
      currentCapacity = energyAvailable (room spawn)
      towers = find (room spawn) find_my_structures # mapMaybe (\(x :: forall a. Structure a) -> toTower x)

    if energyAvailable (room spawn) == totalCapacity && shouldSpawn && canSpawn
    then spawnNewCreeps spawn currentCapacity controllerLevel anyHostiles
    else pure unit

    for towers runTower

  for_ (creeps game) \n -> do
    runCreepRole n

  

