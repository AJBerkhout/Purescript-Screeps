-- | Corresponds to the Screeps API [StructurePowerSpawn](http://support.screeps.com/hc/en-us/articles/208436585-StructurePowerSpawn)
module Screeps.PowerSpawn where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_power_spawn)
import Screeps.FFI (runThisEffFn0, runThisEffFn1, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (PowerSpawn, ReturnCode, Structure)

energy :: PowerSpawn -> Int
energy = unsafeField "energy"

energyCapacity :: PowerSpawn -> Int
energyCapacity = unsafeField "energyCapacity"

power :: PowerSpawn -> Int
power = unsafeField "power"

powerCapacity :: PowerSpawn -> Int
powerCapacity = unsafeField "powerCapacity"

createPowerCreep :: PowerSpawn -> String -> Effect ReturnCode
createPowerCreep spawn name = runThisEffFn1 "createPowerCreep" spawn name

processPower :: PowerSpawn -> Effect ReturnCode
processPower = runThisEffFn0 "processPower"

toPowerSpawn :: forall a. Structure a -> Maybe PowerSpawn
toPowerSpawn = unsafeCast structure_power_spawn
