-- | Corresponds to the Screeps API [StructureSpawn](http://support.screeps.com/hc/en-us/articles/205990342-StructureSpawn)
module Screeps.Spawn where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Screeps.Constants (structure_spawn)
import Screeps.FFI (runThisEffFn1, runThisEffFn2, runThisFn1, toMaybe, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (BodyPartType, Creep, ReturnCode, Spawn, Structure)

type CreepInfo =
  { name :: String
  , needTime :: Int
  , remainingTime :: Int }

energy :: Spawn -> Int
energy = unsafeField "energy"

energyCapacity :: Spawn -> Int
energyCapacity = unsafeField "energyCapacity"

memory :: forall props. Spawn -> { | props }
memory = unsafeField "memory"

name :: Spawn -> String
name = unsafeField "name"

spawning :: Spawn -> Maybe CreepInfo
spawning spawn = toMaybe $ unsafeField "spawning" spawn

canCreateCreep :: Spawn -> Array BodyPartType -> ReturnCode
canCreateCreep spawn parts = runThisFn1 "canCreateCreep" spawn parts

canCreateCreep' :: Spawn -> Array BodyPartType -> String -> Effect ReturnCode
canCreateCreep' spawn parts name' = runThisEffFn2 "canCreateCreep" spawn parts name'

foreign import createCreepImpl :: 
  Spawn ->
  Array BodyPartType ->
  (ReturnCode -> Either ReturnCode String) ->
  (String -> Either ReturnCode String) ->
  Effect (Either ReturnCode String)
foreign import createCreepPrimeImpl :: 
  Spawn -> Array BodyPartType -> String -> { memory :: Json, dryRun :: Boolean } -> Effect ReturnCode

createCreep :: Spawn -> Array BodyPartType -> Effect (Either ReturnCode String)
createCreep spawn parts = createCreepImpl spawn parts Left Right

createCreep' :: 
  forall mem. (EncodeJson mem) =>
  Spawn -> Array BodyPartType -> Maybe String -> { memory :: mem, dryRun :: Boolean } -> Effect ReturnCode
createCreep' spawn parts name' { memory: memry, dryRun } = 
  createCreepPrimeImpl spawn parts (fromMaybe "" name') { memory: encodeJson memry, dryRun }

recycleCreep :: Spawn -> Creep -> Effect ReturnCode
recycleCreep = runThisEffFn1 "recycleCreep"

renewCreep :: Spawn -> Creep -> Effect ReturnCode
renewCreep = runThisEffFn1 "renewCreep"

toSpawn :: forall a. Structure a -> Maybe Spawn
toSpawn = unsafeCast structure_spawn
