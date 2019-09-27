{-# LANGUAGE Rank2Types #-}

module Ivory.BSP.STM32.Peripheral.I2C
  ( I2C(..)
  , I2CPins(..)
  , I2CVersion(..)
  , mkI2CVersion
  ) where

import Ivory.Language
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.I2C.Pins (I2CPins(..))

{{#versions}}
import qualified Ivory.BSP.STM32.Peripheral.I2Cv{{ version }}.Peripheral as P{{ version }}
{{/versions}}

data I2CVersion =
{{#versions}}
  {{ prefix }} V{{ version }}
{{/versions}}

data I2C =
{{#versions}}
  {{ prefix }} WrappedV{{ version }} P{{ version }}.I2C
{{/versions}}

mkI2CVersion
       :: (STM32Interrupt i)
       => I2CVersion
       -> Integer -- Base
       -> (forall eff . Ivory eff ()) -- RCC Enable
       -> (forall eff . Ivory eff ()) -- RCC Disable
       -> (forall eff . Ivory eff ()) -- RCC Reset
       -> i -- event interrupt
       -> i -- error interrupt
       -> String -- Name
       -> I2C
{{#versions}}
mkI2CVersion V{{ version }} i e1 e2 e3 j k s = WrappedV{{ version }} $ P{{ version }}.mkI2C i e1 e2 e3 j k s
{{/versions}}
