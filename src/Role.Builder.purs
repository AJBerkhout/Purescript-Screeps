module Role.Builder (runBuilder, BuilderMemory, Builder) where

import Prelude

import CreepRoles (Role)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_construction_sites, find_sources_active, resource_energy)
import Screeps.Creep (amtCarrying, build, carryCapacity, harvestSource, moveTo, setAllMemory)
import Screeps.Room (find)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByPath)
import Screeps.Types (Creep, FindContext(..), TargetPosition(..))

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
          setMemory builder (mem { working = false })
      false -> do
        site <- findClosestByPath (pos creep) (OfType find_construction_sites)
        case site of
          Right (Just targetSite) -> do
            buildResult <- build creep targetSite
            if buildResult == err_not_in_range 
            then moveTo creep (TargetObj targetSite) # ignoreM
            else pure unit
          _ -> do
            pure unit
  else do
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> do
        setMemory builder (mem { working = true })
      false -> do
        source <- findClosestByPath (pos creep) (OfType find_sources_active)
        case source of
          Right (Just targetSource) -> do
            harvestResult <- harvestSource creep targetSource
            if harvestResult == err_not_in_range
            then moveTo creep (TargetObj targetSource) # ignoreM
            else pure unit
          _ -> pure unit


