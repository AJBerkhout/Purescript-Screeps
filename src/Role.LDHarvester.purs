module Role.LDHarvester (runLDHarvester, LDHarvesterMemory, LDHarvester) where

import Prelude
import CommonActions (collectEnergy, transport)
import CreepRoles (Role)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, moveTo, setAllMemory)
import Screeps.Room (RoomIdentifier(..), findExitTo, name)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByRange)
import Screeps.Types (Creep, FindContext(..), TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type LDHarvesterMemory = { role :: Role, working :: Boolean, targetRoom :: String, home :: String }
type LDHarvester = { creep :: Creep, mem :: LDHarvesterMemory }

setMemory :: LDHarvester -> LDHarvesterMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 
         
runLDHarvester :: LDHarvester -> Effect Unit
runLDHarvester ld@{ creep, mem } =
  if mem.working then
    case (amtCarrying creep resource_energy == 0) of
      false -> 
        case (name (room creep) == mem.home) of
          true -> transport creep
          false ->
            let maybeExit = findExitTo (room creep) (RoomName mem.home) in
            case maybeExit of
              Left e -> 
                do pure unit
              Right exitSearch -> do
                exit <- findClosestByRange (pos creep) (OfType exitSearch)
                case exit of
                  Just position -> do
                    ignoreM (moveTo creep (TargetPos position))
                  _-> do pure unit
      true -> 
        setMemory ld (mem {working = false})
  else 
    case amtCarrying creep resource_energy == carryCapacity creep of
      true -> setMemory ld (mem {working = true})
      false ->
        case (mem.targetRoom == (name (room creep))) of
          true -> do
            collectEnergy creep false
          false -> 
            let maybeExit = findExitTo (room creep) (RoomName mem.targetRoom) in
              case maybeExit of
                Left e -> do
                  do pure unit
                Right exitSearch -> do
                  exit <- findClosestByRange (pos creep) (OfType exitSearch)
                  case exit of
                    Just position -> do
                      ignoreM (moveTo creep (TargetPos position))
                    _ -> do 
                      pure unit
                  