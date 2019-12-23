module Role.Harvester 
  (runHarvester
  , Job(..)
  , HarvesterMemory
  , Harvester
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import CreepTasks (buildNextConstructionSite, deliverToClosestStructure, harvestEnergy, upgradeNearestController)
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
  --   [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work]  -- 2000 (34 extensions)
  -- , [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work]  -- 1800 (30 extensions)
  -- , [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work, part_work]  -- 1600 (26 extensions)
  -- , [ part_move, part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work, part_work] -- 1400 (22 extensions)
  -- , [ part_move, part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work, part_work] -- 1200 (18 extensions)
  -- , [ part_move, part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work, part_work] -- 1000 (14 extensions)
  -- , [ part_move, part_move, part_move, part_move, part_carry, part_carry, part_carry, part_carry, part_work, part_work, part_work, part_work] -- 800 (10 extensions)
  -- , [ part_move, part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work, part_work] -- 600
  [ part_move, part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 550 (5 extensions)
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 500
  , [ part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work ] -- 450
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ] -- 400
  , [ part_move, part_move, part_carry, part_work, part_work ] -- 350
  , [ part_move, part_carry, part_work, part_work ] -- 300 (just the spawn)
  ]

data Job = Delivering | Harvesting
type HarvesterMemory = { role :: Role, job :: Job }
type Harvester = { creep :: Creep, mem :: HarvesterMemory }

instance encodeJobJson :: EncodeJson Job where
  encodeJson Delivering = fromString "delivering"
  encodeJson Harvesting = fromString "harvesting"

instance decodeJobJson :: DecodeJson Job where
  decodeJson json 
    | Just "delivering" <- toString json = Right Delivering
    | Just "harvesting" <- toString json = Right Harvesting
    | otherwise = Left $ "Unable to recognize harvester job: " <> stringify json

setMemory :: Harvester -> HarvesterMemory -> Effect Unit
setMemory { creep } mem =  
  setAllMemory creep mem

runHarvester :: Harvester -> Effect Unit
runHarvester harvester@{ creep, mem } =

  case mem.job of
    Delivering -> 
      if creep `amtCarrying` resource_energy == 0
      then do
        _ <- creep `say` "harvesting"
        setMemory harvester (mem { job = Harvesting })
      else do
        result <- deliverToClosestStructure creep
        if result then pure unit else do
          result2 <- buildNextConstructionSite creep
          if result2 then pure unit else do
            result3 <- upgradeNearestController creep
            if result3 then pure unit
            else creep `say` "I'm stuck" <#> ignore
  
    Harvesting ->
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- creep `say` "delivering"
        setMemory harvester (mem { job = Delivering })
      else harvestEnergy creep <#> ignore
          
