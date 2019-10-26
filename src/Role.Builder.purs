module Role.Builder (runBuilder, BuilderMemory, Builder) where

import Prelude
import CommonActions (buildStructure, collectEnergy)
import CreepRoles (Role)
import Effect (Effect)
import Screeps (resource_energy)
import Screeps.Creep (amtCarrying, carryCapacity, setAllMemory)
import Screeps.Types (Creep)

ignore :: forall a. a -> Unit
ignore _ = unit

ignoreM :: forall m a. Monad m => m a -> m Unit
ignoreM m = m <#> ignore 

type BuilderMemory = { role :: Role, working :: Boolean }
type Builder = { creep :: Creep, mem :: BuilderMemory }

setMemory :: Builder -> BuilderMemory -> Effect Unit
setMemory {creep} mem = setAllMemory creep mem 

runBuilder :: Builder -> Effect Unit
runBuilder builder@{ creep, mem } = do

  if mem.working
  then do
    case ((amtCarrying creep resource_energy) == 0) of
      true -> 
        do
          setMemory builder (mem { working = false })
      false -> 
        buildStructure creep
  else do
    case ((amtCarrying creep resource_energy) == (carryCapacity creep)) of
      true -> do
        setMemory builder (mem { working = true })
      false -> do
        collectEnergy creep true


