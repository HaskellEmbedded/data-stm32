
module Ivory.BSP.STM32.Peripheral.SPI.Pins where

import Ivory.BSP.STM32.Peripheral.GPIO

data SPIPins = SPIPins
  { spiPinMiso     :: GPIOPin
  , spiPinMosi     :: GPIOPin
  , spiPinSck      :: GPIOPin
  , spiPinAF       :: GPIO_AF
  }
