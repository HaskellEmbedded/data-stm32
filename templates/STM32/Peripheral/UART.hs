
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module {{ modns }} (
    UART(..)
  , UARTPins(..)
  , UARTVersion(..)
  , mkUARTVersion

  --  module Ivory.BSP.STM32.Peripheral.UART1.Peripheral
  ) where

import Ivory.Language
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.UART.Pins (UARTPins(..))

{{#versions}}
import qualified Ivory.BSP.STM32.Peripheral.UARTv{{ version }}.Peripheral as P{{ version }}
{{/versions}}

data UARTVersion =
{{#versions}}
  {{ prefix }} V{{ version }}
{{/versions}}

data UART =
{{#versions}}
  {{ prefix }} WrappedV{{ version }} P{{ version }}.UART
{{/versions}}

mkUARTVersion
       :: (STM32Interrupt i)
       => UARTVersion
       -> Integer
       -> (forall eff . Ivory eff ())
       -> (forall eff . Ivory eff ())
       -> i
       -> PClk
       -> String
       -> UART
{{#versions}}
mkUARTVersion V{{ version }} i e1 e2 j c s = WrappedV{{ version }} $ P{{ version }}.mkUART i e1 e2 j c s
{{/versions}}
