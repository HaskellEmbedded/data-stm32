
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

{% for item in versions %}
import qualified Ivory.BSP.STM32.Peripheral.UART{{ item.version }}.Peripheral as P{{ item.version }}
--import qualified Ivory.BSP.STM32.Peripheral.UART@version@.Regs       as P@version@
--import qualified Ivory.BSP.STM32.Peripheral.UART@version@.Types      as P@version@
{% endfor %}
data UARTVersion =
{% for item in versions %}
  {{ item.prefix }} V{{ item.version }}
{% endfor %}

data UART =
{% for item in versions %}
  {{ item.prefix }} WrappedV{{ item.version }} P{{ item.version }}.UART
{% endfor %}

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
{% for item in versions %}
mkUARTVersion V{{ item.version }} i e1 e2 j c s = WrappedV{{ item.version }} $ P{{ item.version }}.mkUART i e1 e2 j c s
{% endfor %}
