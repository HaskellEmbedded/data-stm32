{-# LANGUAGE Rank2Types #-}

module Ivory.BSP.STM32.Peripheral.UART.Pins
  (UARTPins(..)) where

import Ivory.BSP.STM32.Peripheral.GPIO

data UARTPins = UARTPins
  { uartPinTx      :: GPIOPin
  , uartPinRx      :: GPIOPin
  }
