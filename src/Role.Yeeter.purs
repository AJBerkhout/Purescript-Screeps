module Role.Yeeter (runYeeter, YeeterMemory, Yeeter) where

import Prelude

import CommonActions (transport)
import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_dropped_resources, find_my_structures, resource_energy)
import Screeps.Container (storeGet, toContainer)
import Screeps.Creep (amtCarrying, carryCapacity, moveTo, pickup, setAllMemory, withdraw)
import Screeps.Room (find')
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath')
import Screeps.Types (Creep, FindContext(..), Structure, TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type YeeterMemory = { role :: Role, working :: Boolean }
type Yeeter = { creep :: Creep, mem :: YeeterMemory }

setMemory :: Yeeter -> YeeterMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

isNonEmptyContainer :: forall a. Structure a -> Boolean
isNonEmptyContainer struct =
  case toContainer struct of
    Just container ->
      storeGet container resource_energy > 0
    Nothing ->
      false

runYeeter :: Yeeter -> Effect Unit
runYeeter yeeter@{ creep, mem } =
  case mem.working of
    true ->
      case amtCarrying creep resource_energy == carryCapacity creep of
        false -> do
            closestDrop <- findClosestByPath' (pos creep) (OfType find_dropped_resources) (closestPathOpts {ignoreCreeps = Just true})
            case closestDrop of
              Just e -> do
                code <- pickup creep e
                if code == err_not_in_range then
                  moveTo creep (TargetObj e) # ignoreM
                else
                  pure unit
              Nothing -> 
                case head (find' (room creep) find_my_structures isNonEmptyContainer) of
                  Just (container :: forall a. Structure a) -> do
                    code <- withdraw creep container resource_energy
                    if code == err_not_in_range then
                      moveTo creep (TargetObj container) # ignoreM
                    else 
                      pure unit
                  Nothing -> 
                    pure unit
        true ->
          setMemory yeeter (mem {working = false})  
    false ->
      case amtCarrying creep resource_energy == 0 of
        true ->
          setMemory yeeter (mem {working = true})
        false -> 
          transport creep
                
                    
