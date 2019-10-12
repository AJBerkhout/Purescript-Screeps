module Role.Builder (runBuilder) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import CreepRoles (Upgrader)
import Screeps (err_not_in_range, find_construction_sites, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, harvestSource, moveTo, say, setMemory)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Types (TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

runBuilder :: Upgrader -> Effect Unit
runBuilder { creep, mem: { working } } = do

  if working
  then
    case ((amtCarrying creep resource_energy) == 0) of
      true -> 
        do
          s <- say creep "Harvesting"
          setMemory creep "working" false
      false ->
        case head (find (room creep) find_construction_sites) of
          Nothing -> do
            pure unit
          Just targetSite -> do
            buildResult <- build creep targetSite
            if buildResult == err_not_in_range 
            then moveTo creep (TargetObj targetSite) # ignoreM
            else pure unit
  else
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> do
        s <- say creep "working"
        setMemory creep "working" true
      false ->
        case head (find (room creep) find_sources) of
          Nothing -> do
            pure unit
          Just targetSite -> do
            harvest <- harvestSource creep targetSite
            if harvest == err_not_in_range 
            then moveTo creep (TargetObj targetSite) # ignoreM
            else pure unit
              

      