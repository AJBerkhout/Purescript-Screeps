module Role.Builder (runBuilder, BuilderMemory, Builder) where

import Prelude

import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_sources, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, harvestSource, moveTo, say, setAllMemory)
import Screeps.Room (find)
import Screeps.RoomObject (room)
import Screeps.Types (TargetPosition(..), Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type BuilderMemory = { role :: Role, working :: Boolean }
type Builder = { creep :: Creep, mem :: BuilderMemory }

setMemory :: Builder -> BuilderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runBuilder :: Builder -> Effect Unit
runBuilder builder@{ creep, mem } = do

  if mem.working
  then do
    case ((amtCarrying creep resource_energy) == 0) of
      true -> 
        do
          s <- say creep "Harvesting"
          setMemory builder (mem { working = false })
      false ->
        case head (find (room creep) find_construction_sites) of
          Nothing -> do
            pure unit
          Just targetSite -> do
            buildResult <- build creep targetSite
            if buildResult == err_not_in_range 
            then moveTo creep (TargetObj targetSite) # ignoreM
            else pure unit
  else do
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> do
        s <- say creep "working"
        setMemory builder (mem { working = true })
      false -> do
        case head (find (room creep) find_sources) of
          Nothing -> do
            pure unit
          Just targetSite -> do
            harvest <- harvestSource creep targetSite
            if harvest == err_not_in_range 
            then moveTo creep (TargetObj targetSite) # ignoreM
            else pure unit
              

