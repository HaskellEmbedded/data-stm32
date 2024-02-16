{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.BSP.ARMv7M.Instr (
    bkpt
  , dsb
  , isb
  , sev
  , wfi
  , wfe
  , instrArtifacts
  , instrModuleDef
  ) where

-- Misc ARM instructions

import Ivory.Language
import Ivory.Artifact
import qualified Paths_ivory_bsp_stm32 as P

instrModuleDef :: ModuleDef
instrModuleDef = do
  incl bkpt
  incl dsb
  incl isb
  incl sev
  incl wfi
  incl wfe

header :: String
header = "instr.h"

instrArtifacts :: [ Located Artifact ]
instrArtifacts = [ Incl $ artifactCabalFile P.getDataDir ("support/" ++ header) ]

-- | Debug breakpoint (as used by assert)
bkpt :: Def ('[] :-> ())
bkpt = importProc "bkpt" header

-- | Data synchronization barrier
dsb :: Def ('[] :-> ())
dsb = importProc "dsb" header

-- | Instruction synchronization barrier (pipeline flush)
isb :: Def ('[] :-> ())
isb = importProc "isb" header

-- | Signal event to all cores in multiprocessor system
sev :: Def ('[] :-> ())
sev = importProc "sev" header

-- | Wait for interrupt or external event
-- signaled by another processor
wfe :: Def ('[] :-> ())
wfe = importProc "wfe" header

-- | Wait for interrupt
wfi :: Def ('[] :-> ())
wfi = importProc "wfi" header
