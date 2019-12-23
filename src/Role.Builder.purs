module Role.Builder 
  ( runBuilder
  , Job(..)
  , BuilderMemory
  , Builder
  , constructionPlans) where

import Prelude

import CreepRoles (Role)
import CreepTasks (buildNextConstructionSite, harvestEnergy, repairNearestStructure, upgradeNearestController)
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
  , [ part_move, part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 550 (5 extensions)
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work, part_work ] -- 500
  , [ part_move, part_move, part_carry, part_carry, part_carry, part_work, part_work ] -- 450
  , [ part_move, part_move, part_carry, part_carry, part_work, part_work ] -- 400
  , [ part_move, part_move, part_carry, part_work, part_work ] -- 350
  , [ part_move, part_carry, part_work, part_work ] -- 300 (just the spawn)
  , [ part_move, part_carry, part_work ] -- 200
  ]

data Job = Building | Harvesting
type BuilderMemory = { role :: Role, job :: Job }
type Builder = { creep :: Creep, mem :: BuilderMemory }

instance encodeJobJson :: EncodeJson Job where
  encodeJson Building   = fromString "building"
  encodeJson Harvesting = fromString "harvesting"

instance decodeJobJson :: DecodeJson Job where
  decodeJson json 
    | Just "building" <- toString json   = Right Building
    | Just "harvesting" <- toString json = Right Harvesting 
    | otherwise = Left $ "Unable to recognize builder job: " <> stringify json

setMemory :: Builder -> BuilderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runBuilder :: Builder -> Effect Unit
runBuilder builder@{ creep, mem } = do

  case mem.job of
    Building ->
      if creep `amtCarrying` resource_energy == 0 
      then do
        _ <- say creep "harvesting"
        setMemory builder (mem { job = Harvesting })
      else do 
        result <- buildNextConstructionSite creep
        if result then pure unit else do
          result2 <- repairNearestStructure creep
          if result2 then pure unit else do
            result3 <- upgradeNearestController creep
            if result3 then pure unit 
            else creep `say` "I'm stuck" <#> ignore

    Harvesting ->
      if creep `amtCarrying` resource_energy == carryCapacity creep
      then do
        _ <- say creep "building"
        setMemory builder (mem { job = Building })
      else harvestEnergy creep <#> ignore

