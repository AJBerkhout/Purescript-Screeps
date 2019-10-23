module Construction (setupSpawn) where

import Prelude
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Console (log)
import Screeps (find_my_construction_sites, find_my_structures, find_sources_active, structure_road)
import Screeps.Road (toRoad)
import Screeps.Room (controller, createConstructionSite, find, find')
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (findPathTo)
import Screeps.Types (RawRoomObject, Room, TargetPosition(..), Spawn)

setupSpawn :: Spawn -> Effect Unit
setupSpawn spawn = 
  let 
    anySites = length (find (room spawn) find_my_construction_sites) > 0
    anyRoads = length (find' (room spawn) find_my_structures (\n ->isJust (toRoad n))) > 0
  in
    if (not (anySites || anyRoads)) then
      let 
        sources = find (room spawn) find_sources_active
      in do
      x <- 
        (for_ sources \source -> do
        setupRoad (room spawn) spawn source)
      case (controller (room spawn)) of
        Just c -> setupRoad (room spawn) spawn c
        Nothing -> pure unit
    else pure unit



setupRoad :: forall a b. Room -> RawRoomObject a -> RawRoomObject b -> Effect Unit
setupRoad room source target = 
  do
  pathToBuild <- findPathTo (pos source) (TargetObj target) 
  case pathToBuild of
    Left e -> pure unit
    Right path ->
      for_ path \position -> do
        createConstructionSite room (TargetPt position.x position.y) structure_road