module CreepClassification 
  ( CreepMemory(..)
  , VocationalCreep(..)
  , UnknownCreepType(..)
  , classifyCreep
  , spawnCreep) where

import Prelude

import CreepRoles (Role(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Role.Builder (BuilderMemory, Builder)
import Role.Guard (GuardMemory, Guard)
import Role.Harvester (HarvesterMemory, Harvester)
import Role.Healer (HealerMemory, Healer)
import Role.LDHarvester (LDHarvesterMemory, LDHarvester)
import Role.Repairer (RepairerMemory, Repairer)
import Role.Upgrader (UpgraderMemory, Upgrader)
import Role.WallRepairer (WallRepairerMemory, WallRepairer)
import Screeps.Creep (getAllMemory)
import Screeps.Spawn (createCreep')
import Screeps.Types (BodyPartType, Creep, ReturnCode, Spawn)

data CreepMemory 
  = HarvesterMemory HarvesterMemory
  | BuilderMemory BuilderMemory
  | UpgraderMemory UpgraderMemory
  | LDHarvesterMemory LDHarvesterMemory
  | RepairerMemory RepairerMemory
  | WallRepairerMemory WallRepairerMemory
  | HealerMemory HealerMemory
  | GuardMemory GuardMemory

instance encodeCreepMemory :: EncodeJson CreepMemory where
  encodeJson (HarvesterMemory mem) = encodeJson mem
  encodeJson (BuilderMemory mem) = encodeJson mem
  encodeJson (UpgraderMemory mem) = encodeJson mem 
  encodeJson (LDHarvesterMemory mem) = encodeJson mem
  encodeJson (RepairerMemory mem) = encodeJson mem
  encodeJson (WallRepairerMemory mem) = encodeJson mem
  encodeJson (GuardMemory mem) = encodeJson mem
  encodeJson (HealerMemory mem) = encodeJson mem

instance decodeCreepMemory :: DecodeJson CreepMemory where
  decodeJson json = go
    where
      go
        | Right (mem@{role: HarvesterRole}) <- decodeJson json = pure $ HarvesterMemory mem
        | Right (mem@{role: UpgraderRole}) <- decodeJson json = pure $ UpgraderMemory mem
        | Right (mem@{role: BuilderRole}) <- decodeJson json = pure $ BuilderMemory mem
        | Right (mem@{role: LDHarvesterRole}) <- decodeJson json = pure $ LDHarvesterMemory mem
        | Right (mem@{role: RepairerRole}) <- decodeJson json = pure $ RepairerMemory mem
        | Right (mem@{role: WallRepairerRole}) <- decodeJson json = pure $ WallRepairerMemory mem
        | Right (mem@{role: HealerRole}) <- decodeJson json = pure $ HealerMemory mem
        | Right (mem@{role: GuardRole}) <- decodeJson json = pure $ GuardMemory mem
        | Right (mem@{role: WallRepairerRole}) <- decodeJson json = pure $ WallRepairerMemory mem
        | otherwise = Left $ "Unable to decode creep memory: " <> JSON.stringify json

data VocationalCreep = 
  Harvester Harvester 
  | Builder Builder 
  | Upgrader Upgrader 
  | LDHarvester LDHarvester 
  | Repairer Repairer
  | WallRepairer WallRepairer
  | Healer Healer
  | Guard Guard

newtype UnknownCreepType = UnknownCreepType String

classifyCreep :: Creep -> Effect (Either UnknownCreepType VocationalCreep)
classifyCreep creep = do
  mem <- getAllMemory creep 
  case decodeJson mem of
    Right (HarvesterMemory h) -> pure $ Right $ Harvester { creep, mem: h }
    Right (UpgraderMemory u) -> pure $ Right $ Upgrader { creep, mem: u }
    Right (BuilderMemory b) -> pure $ Right $ Builder { creep, mem: b }
    Right (LDHarvesterMemory l) -> pure $ Right $ LDHarvester { creep, mem: l}
    Right (RepairerMemory r) -> pure $ Right $ Repairer {creep, mem: r}
    Right (WallRepairerMemory w) -> pure $ Right $ WallRepairer {creep, mem: w}
    Right (HealerMemory h) -> pure $ Right $ Healer {creep, mem: h}
    Right (GuardMemory g) -> pure $ Right $ Guard {creep, mem: g}
    Left err -> pure $ Left $ UnknownCreepType $ "couldn't classify creep with memory: " <> JSON.stringify mem <> ". " <> err

spawnCreep :: Spawn -> Array BodyPartType -> Maybe String -> CreepMemory -> Effect (Either ReturnCode String)
spawnCreep spawn bodyParts name mem = createCreep' spawn bodyParts name mem
