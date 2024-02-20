{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.STM32.Core where

import GHC.Generics (Generic)
import Data.Serialize
import qualified Data.List as L

import Data.STM32.Family

data Core =
    CortexM0
  | CortexM0Plus
  | CortexM3
  | CortexM33
  | CortexM4F
  | CortexM7F
  | CortexA7
  deriving (Eq, Show, Ord, Generic)

instance Serialize Core

core :: Family -> Core
core F0     = CortexM0
core F1     = CortexM3
core F2     = CortexM3
core F3     = CortexM4F
core F4     = CortexM4F
core F7     = CortexM7F
core G4     = CortexM4F
core H7     = CortexM7F
core L0     = CortexM0Plus
core L1     = CortexM3
core L4     = CortexM4F
core L4Plus = CortexM4F
core L5     = CortexM4F
core WB     = CortexM3
core TS     = CortexM3
core _      = CortexM3

-- | FreeRTOS core name, prefixed with @portable/GCC/ARM_@
-- See https://github.com/FreeRTOS/FreeRTOS-Kernel/tree/main/portable/GCC
freertosCore :: String -> Family -> String
-- only these two match CM7/r0p1 freertos port
freertosCore shortName _family | "F74" `L.isPrefixOf` shortName = "CM7"
freertosCore shortName _family | "F75" `L.isPrefixOf` shortName = "CM7"
freertosCore _shortName family = coreStr $ core family
  where
   coreStr :: Core -> String
   coreStr CortexM0     = "CM0"
   coreStr CortexM0Plus = "CM0"
   coreStr CortexM3     = "CM3"
   coreStr CortexM33    = "CM4F"
   coreStr CortexM4F    = "CM4F"
   coreStr CortexM7F    = "CM4F"
   coreStr CortexA7     = error "freertosCore: Don't know how to handle CortexA7"

-- -mcpu for GCC/LD
cpu :: Core -> String
cpu CortexM0     = "cortex-m0"
cpu CortexM0Plus = "cortex-m0plus"
cpu CortexM3     = "cortex-m3"
cpu CortexM33    = "cortex-m33"
cpu CortexM4F    = "cortex-m4"
cpu CortexM7F    = "cortex-m7"
cpu CortexA7     = "cortex-a7"

-- -mfpu for GCC
fpu :: Core -> String
fpu CortexM4F    = "fpv4-sp-d16"
fpu CortexM7F    = "fpv5-sp-d16"
fpu _            = "auto"

-- -mfloat-abi for GCC
floatabi :: Core -> String
floatabi CortexM4F = "hard"
floatabi CortexM7F = "hard"
floatabi _         = "soft"
