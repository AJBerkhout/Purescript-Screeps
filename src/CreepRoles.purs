module CreepRoles (Role(..)) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data Role 
  = HarvesterRole
  | BuilderRole
  | UpgraderRole
  | LDHarvesterRole

instance showRole :: Show Role where
  show HarvesterRole = "harvester"
  show BuilderRole = "builder"
  show UpgraderRole = "upgrader"
  show LDHarvesterRole = "ldharvester"

instance decodeRole :: DecodeJson Role where
  decodeJson json = ans
    where
      ans
        | jsonStr == (Just $ show HarvesterRole) = Right HarvesterRole
        | jsonStr == (Just $ show BuilderRole) = Right BuilderRole
        | jsonStr == (Just $ show UpgraderRole) = Right UpgraderRole
        | jsonStr == (Just $ show LDHarvesterRole) = Right LDHarvesterRole
        | otherwise = Left $ "unable to parse json as role:\n" <> JSON.stringify json
      
      jsonStr = JSON.toString json

instance encodeRole :: EncodeJson Role where
  encodeJson HarvesterRole = JSON.fromString $ show HarvesterRole
  encodeJson BuilderRole = JSON.fromString $ show BuilderRole 
  encodeJson UpgraderRole = JSON.fromString $ show UpgraderRole
  encodeJson LDHarvesterRole = JSON.fromString $ show LDHarvesterRole
type LDHarvesterMemory = { role :: Role, targetRoom :: String, home :: String}

