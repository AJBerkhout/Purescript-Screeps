module Role.Healer (runHealer, HealerMemory, Healer) where

import Prelude

import CreepRoles (Role)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range, find_my_creeps, find_my_spawns)
import Screeps.Creep (heal, hits, hitsMax, moveTo, setAllMemory)
import Screeps.Room (find, find')
import Screeps.RoomObject (room)
import Screeps.Types (Creep, TargetPosition(..))

type HealerMemory = { role :: Role}
type Healer = { creep :: Creep, mem :: HealerMemory }

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

setMemory :: Healer -> HealerMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runHealer :: Healer -> Effect Unit
runHealer healer@{creep, mem} =
  let 
    damagedCreep = head (find' (room creep) find_my_creeps (\n -> hits n < hitsMax n))
  in do
    case damagedCreep of
      Just c -> do
        healResult <- heal creep c
        if healResult == err_not_in_range then
          moveTo creep (TargetObj c) # ignoreM
        else 
          pure unit
      Nothing -> do
        let maybeSpawn = head (find (room creep) find_my_spawns)
        case maybeSpawn of 
          Just s -> 
            moveTo creep (TargetObj s) # ignoreM
          Nothing -> pure unit

        
