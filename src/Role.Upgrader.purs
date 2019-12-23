module Role.Upgrader 
  ( runUpgrader
  , Job(..)
  , UpgraderMemory
  , Upgrader
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import CreepTasks (harvestEnergy, upgradeNearestController)
import Data.Argonaut (class DecodeJson, class EncodeJson, fromString, stringify, toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screeps (part_carry, part_move, part_work, resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, say, setAllMemory)
import Screeps.Types (BodyPartType, Creep)
import Util (ignore)


constructionPlans :: Array (Array BodyPartType)
constructionPlans =
  [
    [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work]  -- 2000 (34 extensions)
  , [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work]  -- 1800 (30 extensions)
  , [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work]  -- 1600 (26 extensions)
  , [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work] -- 1400 (22 extensions)
  , [ part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work] -- 1200 (18 extensions)
  , [ part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work] -- 1000 (14 extensions)
  , [ part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work] -- 800 (10 extensions)
  , [ part_move, part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work, part_work] -- 600
  , [ part_move, part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 550 (5 extensions)
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 500
  , [ part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work ] -- 450
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ] -- 400
  , [ part_move, part_move, part_carry, part_work, part_work ] -- 350
  , [ part_move, part_carry, part_work, part_work ] -- 300 (just the spawn)
  , [ part_move, part_carry, part_work ] -- 200
  ]

data Job = Upgrading | Harvesting
type UpgraderMemory = { role :: Role, job :: Job }
type Upgrader = { creep :: Creep, mem :: UpgraderMemory }

instance encodeJobJson :: EncodeJson Job where
  encodeJson Upgrading  = fromString "upgrading"
  encodeJson Harvesting = fromString "harvesting"

instance decodeJobJson :: DecodeJson Job where
  decodeJson json 
    | Just "upgrading" <- toString json  = Right Upgrading
    | Just "harvesting" <- toString json = Right Harvesting 
    | otherwise = Left $ "Unable to recognize upgrader job: " <> stringify json

setMemory :: Upgrader -> UpgraderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runUpgrader :: Upgrader -> Effect Unit
runUpgrader upgrader@{ creep, mem } =

  case mem.job of
    Upgrading ->
      if amtCarrying creep resource_energy == 0
      then do
        _ <- creep `say` "harvesting"
        setMemory upgrader (mem { job = Harvesting })
      else do
        result <- upgradeNearestController creep
        if result then pure unit 
        else creep `say` "I'm stuck" <#> ignore

    Harvesting -> 
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- creep `say` "upgrading"
        setMemory upgrader (mem { job = Upgrading }) 
      else harvestEnergy creep <#> ignore
