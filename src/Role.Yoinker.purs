module Role.Yoinker (runYoinker, YoinkerMemory, Yoinker) where

import Prelude

import CreepRoles (Role(..))
import Data.Array (index)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_sources, structure_container)
import Screeps.Creep (harvestSource, moveTo, setAllMemory)
import Screeps.Room (find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (createConstructionSite)
import Screeps.Types (Creep, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type YoinkerMemory = { role :: Role, spawn :: Int }
type Yoinker = { creep :: Creep, mem :: YoinkerMemory }

setMemory :: Yoinker -> YoinkerMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runYoinker :: Yoinker -> Effect Unit
runYoinker yoinker@{ creep, mem } =
  let 
    sources = find (room creep) find_sources
    targetSource = index sources mem.spawn
  in
    case targetSource of
      Nothing -> do
        setMemory yoinker (mem {role = HarvesterRole})
      Just source -> do
        code <- harvestSource creep source
        if code == err_not_in_range then
          moveTo creep (TargetObj source) # ignoreM
        else
          createConstructionSite (pos creep) structure_container # ignoreM

