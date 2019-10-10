-- | Corresponds to the Screeps API [StructureTerminal](http://support.screeps.com/hc/en-us/articles/207713399-StructureTerminal)
module Screeps.Terminal where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_terminal)
import Screeps.FFI (runThisEffFn3, runThisEffFn4, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (ResourceType(ResourceType), ReturnCode, Structure, Terminal)

foreign import data Store :: Type

store :: Terminal -> Store
store = unsafeField "store"

storeGet :: Store -> ResourceType -> Int
storeGet s (ResourceType res) = unsafeField res s

storeCapacity :: Terminal -> Int
storeCapacity = unsafeField "storeCapacity"

send :: Terminal -> ResourceType -> Int -> String -> Effect ReturnCode
send term res amount destRoomName = runThisEffFn3 "send" term res amount destRoomName

send' :: Terminal -> ResourceType -> Int -> String -> String -> Effect ReturnCode
send' term res amount destRoomName description = runThisEffFn4 "send" term res amount destRoomName description

toTerminal :: forall a. Structure a -> Maybe Terminal
toTerminal = unsafeCast structure_terminal
