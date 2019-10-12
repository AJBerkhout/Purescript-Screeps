-- | Corresponds to the Screeps APIs [Memory](http://support.screeps.com/hc/en-us/articles/203084991-API-Reference) and [RawMemory](http://support.screeps.com/hc/en-us/articles/205619121-RawMemory)
module Screeps.Memory where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)

import Effect (Effect)

import Screeps.FFI (runThisEffFn0, runThisEffFn1, unsafeGetFieldEff, unsafeSetFieldEff, unsafeDeleteFieldEff)

foreign import data MemoryGlobal :: Type
foreign import getMemoryGlobal :: Effect MemoryGlobal

foreign import data RawMemoryGlobal :: Type
foreign import getRawMemoryGlobal :: Effect RawMemoryGlobal

get :: forall a. (DecodeJson a) => MemoryGlobal -> String -> Effect (Either String a)
get memoryGlobal key = decodeJson <$> unsafeGetFieldEff key memoryGlobal

set :: forall a. (EncodeJson a) => MemoryGlobal -> String -> a -> Effect Unit
set memoryGlobal key val = unsafeSetFieldEff key memoryGlobal (encodeJson val)

delete :: MemoryGlobal -> String -> Effect Unit
delete memoryGlobal key = unsafeDeleteFieldEff key memoryGlobal

getRaw :: forall a. (DecodeJson a) => RawMemoryGlobal -> Effect (Either String a)
getRaw rawMemoryGlobal = fromJson <$> runThisEffFn0 "get" rawMemoryGlobal

getRaw' :: RawMemoryGlobal -> Effect String
getRaw' rawMemoryGlobal = runThisEffFn0 "get" rawMemoryGlobal

setRaw :: forall a. (EncodeJson a) => RawMemoryGlobal -> a -> Effect Unit
setRaw rawMemoryGlobal memory = runThisEffFn1 "set" rawMemoryGlobal (toJson memory)

setRaw' :: RawMemoryGlobal -> String -> Effect Unit
setRaw' = runThisEffFn1 "set"

fromJson :: forall a. (DecodeJson a) => String -> (Either String a)
fromJson jsonStr = jsonParser jsonStr >>= decodeJson

toJson :: forall a. EncodeJson a => a -> Json
toJson = encodeJson
