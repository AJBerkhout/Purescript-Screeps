module Main (loop) where

import Prelude

import CreepRoles (UnknownCreepType(..), VocationalCreep(..), classifyCreep)
import CreepSpawning (spawnCreepIfNeeded)

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

import Effect (Effect)

import Effect.Console (log)
import Role.Builder (runBuilder)
import Role.Harvester (runHarvester)
import Role.Upgrader (runUpgrader)
import Screeps.Constants (find_my_structures)
import Screeps.Defense (runTower)
import Screeps.Game (creeps, getGameGlobal, spawns)
import Screeps.Room (find')
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
    let towers = find' (room spawn) find_my_structures isTower
    for_ (towers) \n -> do runTower n
    spawnCreepIfNeeded spawn

  for_ (creeps game) \n -> do
    runCreepRole n
    

