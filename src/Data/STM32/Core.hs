{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.STM32.Core where

import GHC.Generics (Generic)
import Data.Serialize
import qualified Data.List as L

import Data.STM32.Family

data Core = CortexM0 | CortexM0Plus | CortexM3 | CortexM4F | CortexM7F | CortexA7
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

freertosCore :: String -> Family -> String
-- only these two match CM7/r0p1 freertos port
freertosCore shortName family | "F74" `L.isPrefixOf` shortName = "CM7F"
freertosCore shortName family | "F75" `L.isPrefixOf` shortName = "CM7F"
freertosCore shortName family | otherwise = coreStr $ core family
  where
   coreStr :: Core -> String
   coreStr CortexM0     = "CM0"
   coreStr CortexM0Plus = "CM0"
   coreStr CortexM3     = "CM3"
   coreStr CortexM4F    = "CM4F"
   coreStr CortexM7F    = "CM4F"

-- -mcpu for GCC/LD
cpu :: Core -> String
cpu CortexM0     = "cortex-m0"
cpu CortexM0Plus = "cortex-m0plus"
cpu CortexM3     = "cortex-m3"
cpu CortexM4F    = "cortex-m4"
cpu CortexM7F    = "cortex-m7"

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
