-- | Corresponds to the Screeps API [StructureLink](http://support.screeps.com/hc/en-us/articles/208436275-StructureLink)
module Screeps.Link where

import Effect
import Data.Maybe (Maybe)

import Screeps.Constants (structure_link)
import Screeps.FFI (runThisEffFn1, runThisEffFn2, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Link, ReturnCode, Structure)

cooldown :: Link -> Int
cooldown = unsafeField "cooldown"

energy :: Link -> Int
energy = unsafeField "energy"

energyCapacity :: Link -> Int
energyCapacity = unsafeField "energyCapacity"

transferEnergy :: Link -> Link -> Effect ReturnCode
transferEnergy = runThisEffFn1 "transferEnergy"

transferEnergyAmt :: Link -> Link -> Int -> Effect ReturnCode
transferEnergyAmt = runThisEffFn2 "transferEnergy"

toLink :: forall a. Structure a -> Maybe Link
toLink = unsafeCast structure_link
