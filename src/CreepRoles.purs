module CreepRoles 
  ( Role(..)
  , CreepMemory(..)
  , HarvesterMemory
  , BuilderMemory
  , UpgraderMemory
  , Harvester
  , Builder
  , Upgrader
  , VocationalCreep(..)
  , UnknownCreepType(..)
  , classifyCreep
  , spawnCreep) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, encodeJson)
import Data.Argonaut as JSON
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Screeps.Creep (getMemory)
import Screeps.Spawn (createCreep')
import Screeps.Types (BodyPartType, Creep, ReturnCode, Spawn)


data Role 
  = HarvesterRole
  | BuilderRole
  | UpgraderRole

instance encodeRole :: EncodeJson Role where
  encodeJson HarvesterRole = JSON.fromString "\"Harvester\""
  encodeJson BuilderRole = JSON.fromString "\"Builder\""
  encodeJson UpgraderRole = JSON.fromString "\"Upgrader\""

instance decodeRole :: DecodeJson Role where
  decodeJson json 
    | "\"Harvester\"" <- JSON.stringify json = Right HarvesterRole
    | "\"Builder\"" <- JSON.stringify json = Right BuilderRole
    | "\"Upgrader\"" <- JSON.stringify json = Right UpgraderRole
    | otherwise = Left $ "unable to parse json as role:\n" <> JSON.stringify json

type HarvesterMemory = { role :: Role }
type BuilderMemory = { role :: Role, working :: Boolean }
type UpgraderMemory = { role :: Role, working :: Boolean }

data CreepMemory 
  = HarvesterMemory HarvesterMemory
  | BuilderMemory BuilderMemory
  | UpgraderMemory UpgraderMemory

instance encodeCreepMemory :: EncodeJson CreepMemory where
  encodeJson (HarvesterMemory mem) = encodeJson mem
  encodeJson (BuilderMemory mem) = encodeJson mem
  encodeJson (UpgraderMemory mem) = encodeJson mem 

type Harvester = { creep :: Creep, mem :: HarvesterMemory }
type Builder = { creep :: Creep, mem :: BuilderMemory }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }

data VocationalCreep = Harvester Harvester | Builder Builder | Upgrader Upgrader

newtype UnknownCreepType = UnknownCreepType String

classifyCreep :: Creep -> Effect (Either UnknownCreepType VocationalCreep)
classifyCreep creep = do
  role <- getMemory creep "role"
  case role of
    Right HarvesterRole -> pure $ Right $ Harvester { creep, mem: { role: HarvesterRole } }
    Right BuilderRole -> do 
      isWorking <- getMemory creep "working"
      pure $ bimap (UnknownCreepType) 
        (\working -> Builder { creep, mem: { role: BuilderRole, working } })
        isWorking
    Right UpgraderRole -> do
      isWorking <- getMemory creep "working"
      pure $ bimap (UnknownCreepType)
        (\working -> Upgrader { creep, mem: { role: UpgraderRole, working } })
        isWorking
    Left err -> pure $ Left $ UnknownCreepType err

spawnCreep :: Spawn -> Array BodyPartType -> Maybe String -> CreepMemory -> Effect (Either ReturnCode String)
spawnCreep spawn bodyParts name mem = createCreep' spawn bodyParts name mem
