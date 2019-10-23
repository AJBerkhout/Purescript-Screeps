module CreepSpawning where

import Prelude

import CreepClassification (CreepMemory(..), VocationalCreep(..), classifyCreep, spawnCreep)
import CreepRoles (Role(..))
import Data.Array (concat, difference, fromFoldable, head, length, mapMaybe, range, (..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Screeps.Constants (find_sources, part_attack, part_carry, part_heal, part_move, part_tough, part_work)
import Screeps.Game (creeps, getGameGlobal)
import Screeps.Map (getAdjacentRooms)
import Screeps.Room (energyAvailable, energyCapacityAvailable, find, name)
import Screeps.RoomObject (room)
import Screeps.Spawn (spawning)
import Screeps.Types (ReturnCode, Spawn)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

spawnCreepIfNeeded :: Spawn -> Boolean -> Effect Unit
spawnCreepIfNeeded spawn battleStations =
  let 
    minHarvesters = 1
    minUpgraders = 2
    minBuilders = 1
    minRepairers = 1
    minWallRepairers = 1
    minLDHarvesters = 3
    minHealers = 
      if battleStations then 
        1
      else 0
    minGuards =
      if battleStations then
        1
      else 0
    minYeetersAndYoinkers = length (find (room spawn) find_sources)
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
        repairers = creepsAndRoles # mapMaybe (case _ of
          (Right (Repairer r)) -> Just r
          _ -> Nothing)
        wallRepairers = creepsAndRoles # mapMaybe (case _ of
          (Right (WallRepairer r)) -> Just r
          _ -> Nothing)
        ldHarvesters = creepsAndRoles # mapMaybe (case _ of 
          (Right (LDHarvester b)) -> Just b
          _ -> Nothing)
        guards = creepsAndRoles # mapMaybe (case _ of 
          (Right (Guard b)) -> Just b
          _ -> Nothing)
        healers = creepsAndRoles # mapMaybe (case _ of 
          (Right (Healer b)) -> Just b
          _ -> Nothing)
        yeeters = creepsAndRoles # mapMaybe (case _ of 
          (Right (Yeeter b)) -> Just b
          _ -> Nothing)
        yoinkers = creepsAndRoles # mapMaybe (case _ of 
          (Right (Yoinker b)) -> Just b
          _ -> Nothing)
        maxEnergy = (energyCapacityAvailable (room spawn))
      if maxEnergy / 2 < (energyAvailable (room spawn)) then
        case (spawning spawn) of
          Nothing -> 
            if (length harvesters < minHarvesters) then
              createBalancedCreep spawn (energyAvailable (room spawn)) (HarvesterMemory {role: HarvesterRole, working: true})
              >>= logShow
            else if ((length guards) < minGuards && (energyAvailable (room spawn)) >= 300) then
              createGuard spawn (energyAvailable (room spawn)) (GuardMemory {role: GuardRole})
              >>= logShow
            else if ((length healers) < minHealers && (energyAvailable (room spawn)) >= 310) then
              createHealer spawn (energyAvailable (room spawn)) (GuardMemory {role: HealerRole})
              >>= logShow
            else if ((length ldHarvesters) < minLDHarvesters) then
              createLDHarvesterForAdjacentRoom spawn (energyAvailable (room spawn))
            else if ((length builders) < minBuilders) then
              createBalancedCreep spawn (energyAvailable (room spawn)) (BuilderMemory {role: BuilderRole, working: true})
              >>= logShow
            else if ((length yoinkers) < minYeetersAndYoinkers && (energyAvailable (room spawn)) >= 300) then
              let
                allYoinkers = 
                  creepsAndRoles # mapMaybe (case _ of 
                    (Right (Yoinker b)) -> Just b
                    _ -> Nothing)
                targetSpawns = 
                  difference (range 0 (length (find (room spawn) find_sources) - 1)) (map (\n -> n.mem.spawn) yoinkers)
              in
              do 
                logShow targetSpawns
                case head (targetSpawns) of
                  Just s ->
                    createYoinker spawn (energyAvailable (room spawn)) (YoinkerMemory {role: YoinkerRole, spawn: s})
                    >>= logShow
                  Nothing -> do
                    pure unit
            else if ((length yeeters) < minYeetersAndYoinkers && (energyAvailable (room spawn)) >= 300) then
              createYeeter spawn (energyAvailable (room spawn)) (YeeterMemory {role: YeeterRole, working: true})
              >>= logShow
            else if ((length upgraders) < minUpgraders) then
              createBalancedCreep spawn (energyAvailable (room spawn)) (UpgraderMemory {role: UpgraderRole, working: true})
              >>= logShow
            else if ((length repairers) < minRepairers) then
              createBalancedCreep spawn (energyAvailable (room spawn)) (RepairerMemory {role: RepairerRole, working: true})
              >>= logShow
            else if ((length wallRepairers) < minWallRepairers) then
              createBalancedCreep spawn (energyAvailable (room spawn)) (WallRepairerMemory {role: WallRepairerRole, working: true})
              >>= logShow
            else 
              pure unit
          Just x -> 
            pure unit
    else do pure unit

createLDHarvesterForAdjacentRoom :: Spawn -> Int -> Effect Unit
createLDHarvesterForAdjacentRoom spawn energy =
  let 
    noName = Nothing
    adjRooms = getAdjacentRooms (name (room spawn))
    workParts = [part_work, part_work]
    numberOtherParts = (energy - 200) / 100
    moveParts = map (\n -> part_move) (0..(numberOtherParts-1))
    carryParts = map (\n -> part_carry) (0..(numberOtherParts-1))
    allParts = concat [workParts, moveParts, carryParts]
  in 
    do
      case adjRooms.topRoom of
        Just targetRoom ->
          spawnCreep spawn allParts noName (LDHarvesterMemory {role: LDHarvesterRole, home: (name (room spawn)), targetRoom, working:true})
          >>= logShow
        Nothing ->
          case adjRooms.leftRoom of
            Just targetRoom ->
              spawnCreep spawn allParts noName (LDHarvesterMemory {role: LDHarvesterRole, home: (name (room spawn)), targetRoom, working:true})
              >>= logShow
            Nothing ->
              case adjRooms.rightRoom of
                Just targetRoom ->
                  spawnCreep spawn allParts noName (LDHarvesterMemory {role: LDHarvesterRole, home: (name (room spawn)), targetRoom, working:true})
                  >>= logShow
                Nothing ->
                  case adjRooms.bottomRoom of
                    Just targetRoom ->
                      spawnCreep spawn allParts noName (LDHarvesterMemory {role: LDHarvesterRole, home: (name (room spawn)), targetRoom, working:true})
                      >>= logShow
                    Nothing ->
                      pure unit

createBalancedCreep :: Spawn -> Int -> CreepMemory -> Effect (Either ReturnCode String)
createBalancedCreep spawn energy mem =
  let 
    numberOfParts = (energy-100) / 200
    moveParts = map (\n -> part_move) (0..(numberOfParts-1))
    workParts = map (\n -> part_carry) (0..(numberOfParts-1))
    carryParts = map (\n -> part_work) (0..(numberOfParts-1))
    allParts = concat [[part_work], moveParts, workParts, carryParts]
    noName = Nothing
  in do
    spawnCreep spawn allParts noName mem

createHealer :: Spawn -> Int -> CreepMemory -> Effect (Either ReturnCode String)
createHealer spawn energy mem =
  let 
    energyExceptHeal = energy - 250
    numberOfParts = energyExceptHeal / (60)
    moveParts = map (\n -> part_move) (0..(numberOfParts-1))
    toughParts = map (\n -> part_tough) (0..(numberOfParts-1))
    allParts = concat [toughParts, [part_heal], moveParts]
    noName = Nothing
  in do
    spawnCreep spawn allParts noName mem

createGuard :: Spawn -> Int -> CreepMemory -> Effect (Either ReturnCode String)
createGuard spawn energy mem =
  let 
    energyExceptAttack = energy - 240
    numberOfParts = energyExceptAttack / (60)
    moveParts = map (\n -> part_move) (0..(numberOfParts-1))
    toughParts = map (\n -> part_tough) (0..(numberOfParts-1))
    allParts = concat [toughParts, [part_attack, part_attack, part_attack], moveParts]
    noName = Nothing
  in do
    spawnCreep spawn allParts noName mem

createYeeter :: Spawn -> Int -> CreepMemory -> Effect (Either ReturnCode String)
createYeeter spawn energy mem =
  let 
    numberOfParts = energy / 100
    moveParts = map (\n -> part_move) (0..(numberOfParts-1))
    carryParts = map (\n -> part_carry) (0..(numberOfParts-1))
    allParts = concat [carryParts, moveParts]
    noName = Nothing
  in do
    spawnCreep spawn allParts noName mem

createYoinker :: Spawn -> Int -> CreepMemory -> Effect (Either ReturnCode String)
createYoinker spawn energy mem =
  let 
    numberOfParts = (energy - 50) / 100
    workParts = map (\n -> part_work) (0..(numberOfParts-1))
    allParts = concat [[part_move], workParts]
    noName = Nothing
  in do
    logShow energy  
    logShow allParts  
    spawnCreep spawn allParts noName mem
