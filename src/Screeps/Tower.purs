-- | Corresponds to the Screeps API [StructureTower](http://support.screeps.com/hc/en-us/articles/208437105-StructureTower)
module Screeps.Tower where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_tower)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Creep, ReturnCode, Structure, Tower)
import Screeps.FFI (runThisEffFn1, runThisEffFn2, unsafeField)

energy :: Tower -> Int
energy = unsafeField "energy"

energyCapacity :: Tower -> Int
energyCapacity = unsafeField "energyCapacity"

attack :: Tower -> Creep -> Effect ReturnCode
attack = runThisEffFn1 "attack"

heal :: Tower -> Creep -> Effect ReturnCode
heal = runThisEffFn1 "heal"

repair :: forall a. Tower -> Structure a -> Effect ReturnCode
repair = runThisEffFn1 "repair"

transferEnergy :: Tower -> Creep -> Effect ReturnCode
transferEnergy = runThisEffFn1 "transferEnergy"

transferEnergyAmt :: Tower -> Creep -> Int -> Effect ReturnCode
transferEnergyAmt = runThisEffFn2 "transferEnergy"

toTower :: forall a. Structure a -> Maybe Tower
toTower = unsafeCast structure_tower
