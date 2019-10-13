module Role.Repairer (runRepairer, RepairerMemory, Repairer) where

import Prelude

import CreepRoles (Role)
import Data.Array (filter, head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Screeps (err_not_in_range, find_construction_sites, find_sources, find_structures, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, harvestSource, moveTo, repair, setAllMemory, transferToStructure)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Structure (hits, hitsMax)
import Screeps.Types (Creep, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type RepairerMemory = { role :: Role, working :: Boolean }
type Repairer = { creep :: Creep, mem :: RepairerMemory }

setMemory :: Repairer -> RepairerMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runRepairer :: Repairer -> Effect Unit
runRepairer repairer@{ creep, mem } = do

  if mem.working
  then do
    case ((amtCarrying creep resource_energy) == 0) of
      true -> 
        do
          setMemory repairer (mem { working = false })
      false ->
        case head (filter (\n -> hits n < hitsMax n )(find (room creep) find_structures)) of
          Nothing -> 
            case head (find (room creep) find_construction_sites) of
              Nothing -> do
                pure unit
              Just targetSite -> do
                buildResult <- build creep targetSite
                if buildResult == err_not_in_range 
                then moveTo creep (TargetObj targetSite) # ignoreM
                else pure unit
          Just damagedBuilding -> do
            repairResult <- repair creep damagedBuilding
            if repairResult == err_not_in_range then
              moveTo creep (TargetObj damagedBuilding) # ignoreM
            else do 
              pure unit
  else do
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> do
        setMemory repairer (mem { working = true })
      false -> do
        case head (find (room creep) find_sources) of
          Nothing -> do
            pure unit
          Just targetSite -> do
            harvest <- harvestSource creep targetSite
            if harvest == err_not_in_range 
            then moveTo creep (TargetObj targetSite) # ignoreM
            else pure unit
              

