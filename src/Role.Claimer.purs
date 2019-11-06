module Role.Claimer(runClaimer, ClaimerMemory, Claimer) where

import Prelude


import CreepRoles (Role)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (err_not_in_range)
import Screeps.Creep (claimController, moveTo, setAllMemory)
import Screeps.Room (RoomIdentifier(..), controller, findExitTo, name)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findClosestByRange)
import Screeps.Types (Creep, FindContext(..), TargetPosition(..))

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type ClaimerMemory = { role :: Role, targetRoom :: String, homeRoom :: String }
type Claimer = { creep :: Creep, mem :: ClaimerMemory }

setMemory :: Claimer -> ClaimerMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runClaimer :: Claimer -> Effect Unit
runClaimer claimer@{creep, mem} =
  case (name (room creep)) == mem.targetRoom of
    true -> do
      case controller (room creep) of
        Just controller -> do
          claimResult <- claimController creep controller
          if (claimResult == err_not_in_range) then
            moveTo creep (TargetObj controller) # ignoreM
          else
            pure unit
        Nothing ->
          setMemory claimer (mem {targetRoom = mem.homeRoom})
    false -> do
      let maybeExit = findExitTo (room creep) (RoomName mem.targetRoom)
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
      