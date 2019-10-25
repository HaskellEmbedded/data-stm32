{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module {{ modns }}
  ( uartTower
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.UART

{{#versions}}
import qualified Ivory.BSP.STM32.Driver.UARTv{{ version }} as V{{ version }}
{{/versions}}

uartTower :: IvoryString s
          => (e -> ClockConfig)
          -> UART
          -> UARTPins
          -> Integer
          -> Tower e ( BackpressureTransmit s ('Stored IBool)
                     , ChanOutput ('Stored Uint8)
                     , Monitor e ())
uartTower tocc uart pins baud = case uart of
  {{#versions}}
  (WrappedV{{ version }} x) -> V{{ version }}.uartTower tocc x pins baud
  {{/versions}}
