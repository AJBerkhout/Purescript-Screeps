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
  | RepairerRole
  | WallRepairerRole
  | HealerRole
  | GuardRole
  | YeeterRole
  | YoinkerRole
  | ClaimerRole

instance showRole :: Show Role where
  show HarvesterRole = "harvester"
  show BuilderRole = "builder"
  show UpgraderRole = "upgrader"
  show LDHarvesterRole = "ldharvester"
  show RepairerRole = "repairer"
  show WallRepairerRole = "wallrepairer"
  show HealerRole = "healer"
  show GuardRole = "guard"
  show YeeterRole = "yeeter"
  show YoinkerRole = "yoinker"
  show ClaimerRole = "claimer"

instance decodeRole :: DecodeJson Role where
  decodeJson json = ans
    where
      ans
        | jsonStr == (Just $ show HarvesterRole) = Right HarvesterRole
        | jsonStr == (Just $ show BuilderRole) = Right BuilderRole
        | jsonStr == (Just $ show UpgraderRole) = Right UpgraderRole
        | jsonStr == (Just $ show LDHarvesterRole) = Right LDHarvesterRole
        | jsonStr == (Just $ show RepairerRole) = Right RepairerRole
        | jsonStr == (Just $ show WallRepairerRole) = Right WallRepairerRole
        | jsonStr == (Just $ show HealerRole) = Right HealerRole
        | jsonStr == (Just $ show GuardRole) = Right GuardRole
        | jsonStr == (Just $ show YeeterRole) = Right YeeterRole
        | jsonStr == (Just $ show YoinkerRole) = Right YoinkerRole
        | jsonStr == (Just $ show ClaimerRole) = Right ClaimerRole
        | otherwise = Left $ "unable to parse json as role:\n" <> JSON.stringify json
      
      jsonStr = JSON.toString json

instance encodeRole :: EncodeJson Role where
  encodeJson HarvesterRole = JSON.fromString $ show HarvesterRole
  encodeJson BuilderRole = JSON.fromString $ show BuilderRole 
  encodeJson UpgraderRole = JSON.fromString $ show UpgraderRole
  encodeJson LDHarvesterRole = JSON.fromString $ show LDHarvesterRole
  encodeJson RepairerRole = JSON.fromString $ show RepairerRole
  encodeJson WallRepairerRole = JSON.fromString $ show WallRepairerRole
  encodeJson HealerRole = JSON.fromString $ show HealerRole
  encodeJson GuardRole = JSON.fromString $ show GuardRole
  encodeJson YeeterRole = JSON.fromString $ show YeeterRole
  encodeJson YoinkerRole = JSON.fromString $ show YoinkerRole
  encodeJson ClaimerRole = JSON.fromString $ show ClaimerRole

