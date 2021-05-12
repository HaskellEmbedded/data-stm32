module Data.STM32.Types (
    NamedMCU
  , module Data.STM32.Core
  , module Data.STM32.Family
  , module Data.STM32.Name
  , module Data.STM32.Periph
  ) where

import Data.CMX.Types (MCU)

import Data.STM32.Core
import Data.STM32.Family
import Data.STM32.Name
import Data.STM32.Periph

type NamedMCU = (STM32DevName, MCU)
