{-# LANGUAGE Rank2Types #-}

module @modns@
  (UARTPins(..)) where

import Ivory.Language
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.GPIO
-- XXX
import Ivory.BSP.STM32.Peripheral.GPIO2.RegTypes

data UARTPins = UARTPins
  { uartPinTx      :: GPIOPin
  , uartPinRx      :: GPIOPin
  , uartPinAF      :: GPIO_AF
  }
