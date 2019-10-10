-- | Corresponds to the Screeps API [StructureObserver](http://support.screeps.com/hc/en-us/articles/208436365-StructureObserver)
module Screeps.Observer where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_observer)
import Screeps.FFI (runThisEffFn1)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Observer, ReturnCode, Structure)

observeRoom :: Observer -> String -> Effect ReturnCode
observeRoom obs roomName = runThisEffFn1 "observeRoom" obs roomName

toObserver :: forall a. Structure a -> Maybe Observer
toObserver = unsafeCast structure_observer
