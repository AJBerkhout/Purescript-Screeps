module Screeps.Ruin where

import Screeps.FFI (unsafeField)
import Screeps.Types (Ruin)

energy :: Ruin -> Int
energy ruin = unsafeField "energy" store
  where store = unsafeField "store" ruin
  