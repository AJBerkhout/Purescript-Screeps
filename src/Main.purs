module Main (loop) where

import Prelude

import Construction (setupSpawn)
import CreepClassification (UnknownCreepType(..), VocationalCreep(..), classifyCreep)
import CreepSpawning (spawnCreepIfNeeded)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Role.Builder (runBuilder)
import Role.Guard (runGuard)
import Role.Harvester (runHarvester)
import Role.Healer (runHealer)
import Role.LDHarvester (runLDHarvester)
import Role.Repairer (runRepairer)
import Role.Upgrader (runUpgrader)
import Role.WallRepairer (runWallRepairer)
import Role.Yeeter (runYeeter)
import Role.Yoinker (runYoinker)
import Screeps.Constants (find_hostile_creeps, find_my_structures)
import Screeps.Defense (runTower)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Room (find, find')
import Screeps.RoomObject (room)
import Screeps.Tower (toTower)
import Screeps.Types (Creep, Structure)

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
matchUnit (Right (LDHarvester creep)) = runLDHarvester creep
matchUnit (Right (Repairer creep)) = runRepairer creep
matchUnit (Right (WallRepairer creep)) = runWallRepairer creep
matchUnit (Right (Healer creep)) = runHealer creep
matchUnit (Right (Guard creep)) = runGuard creep
matchUnit (Right (Yeeter creep)) = runYeeter creep
matchUnit (Right (Yoinker creep)) = runYoinker creep
matchUnit (Left (UnknownCreepType err)) = log $ "One of the creeps has a memory I can't parse.\n" <> err

runCreepRole :: Creep -> Effect Unit
runCreepRole creep = classifyCreep creep >>= matchUnit  
     
   
isTower :: forall a. Structure a -> Boolean
isTower struct =
  case toTower struct of
    Nothing-> false
    Just s -> true

loop :: Effect Unit
loop = do
  
  game <- getGameGlobal
  for_ (spawns game) \spawn -> do
    do setupSpawn spawn
    let 
      towers = find' (room spawn) find_my_structures isTower
      battleStations = length (find (room spawn) find_hostile_creeps) > 0 
    
    for_ (towers) \n -> do runTower n
    spawnCreepIfNeeded spawn battleStations

  for_ (creeps game) \n -> do
    runCreepRole n
    

