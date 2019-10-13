module CreepRoles 
  ( Role(..)
  , CreepMemory(..)
  , HarvesterMemory
  , BuilderMemory
  , UpgraderMemory
  , LDHarvesterMemory
  , Harvester
  , Builder
  , Upgrader
  , LDHarvester
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
import Effect.Console (log)
import Screeps.Creep (getMemory)
import Screeps.Spawn (createCreep')
import Screeps.Types (BodyPartType, Creep, ReturnCode, Spawn)


data Role 
  = HarvesterRole
  | BuilderRole
  | UpgraderRole
  | LDHarvesterRole

instance encodeRole :: EncodeJson Role where
  encodeJson HarvesterRole = JSON.fromString "\"Harvester\""
  encodeJson BuilderRole = JSON.fromString "\"Builder\""
  encodeJson UpgraderRole = JSON.fromString "\"Upgrader\""
  encodeJson LDHarvesterRole = JSON.fromString "\"LDHarvester\""

instance decodeRole :: DecodeJson Role where
  decodeJson json 
    | "\"Harvester\"" <- JSON.stringify json = Right HarvesterRole
    | "\"Builder\"" <- JSON.stringify json = Right BuilderRole
    | "\"Upgrader\"" <- JSON.stringify json = Right UpgraderRole
    | "\"LDHarvester\"" <- JSON.stringify json = Right LDHarvesterRole
    | otherwise = Left $ "unable to parse json as role:\n" <> JSON.stringify json

type HarvesterMemory = { role :: Role }
type BuilderMemory = { role :: Role, working :: Boolean }
type UpgraderMemory = { role :: Role, working :: Boolean }
type LDHarvesterMemory = { role :: Role, targetRoom :: String, home :: String}

data CreepMemory 
  = HarvesterMemory HarvesterMemory
  | BuilderMemory BuilderMemory
  | UpgraderMemory UpgraderMemory
  | LDHarvesterMemory LDHarvesterMemory

instance encodeCreepMemory :: EncodeJson CreepMemory where
  encodeJson (HarvesterMemory mem) = encodeJson mem
  encodeJson (BuilderMemory mem) = encodeJson mem
  encodeJson (UpgraderMemory mem) = encodeJson mem 
  encodeJson (LDHarvesterMemory mem) = encodeJson mem

type Harvester = { creep :: Creep, mem :: HarvesterMemory }
type Builder = { creep :: Creep, mem :: BuilderMemory }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }
type LDHarvester = {creep :: Creep, mem :: LDHarvesterMemory}

data VocationalCreep = Harvester Harvester | Builder Builder | Upgrader Upgrader | LDHarvester LDHarvester

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
    Right LDHarvesterRole -> do
      target <- getMemory creep "targetRoom"
      homeRoom <- getMemory creep "home"
      case target of
        Left e -> do
          pure $ Left $ UnknownCreepType ""
        Right targetRoom ->
          case homeRoom of
            Left e -> do
              pure $ Left $ UnknownCreepType ""
            Right home -> do
              pure $ Right $ LDHarvester {creep, mem: {role: LDHarvesterRole, targetRoom, home}}
    Left err -> do
      pure $ Left $ UnknownCreepType err

spawnCreep :: Spawn -> Array BodyPartType -> Maybe String -> CreepMemory -> Effect (Either ReturnCode String)
spawnCreep spawn bodyParts name mem = createCreep' spawn bodyParts name mem
