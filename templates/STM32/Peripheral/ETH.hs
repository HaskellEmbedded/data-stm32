module Ivory.BSP.STM32.Peripheral.ETH
  ( module Ivory.BSP.STM32.Peripheral.ETH.Descriptor
  , module Ivory.BSP.STM32.Peripheral.ETH.Descriptor.Types
  , module Ivory.BSP.STM32.Peripheral.ETH.Peripheral
  , module Ivory.BSP.STM32.Peripheral.ETH.Pins
  , module Ivory.BSP.STM32.Peripheral.ETH.Phy
  , ETHConfig(..)
  ) where

import Ivory.BSP.STM32.Peripheral.ETH.Descriptor
import Ivory.BSP.STM32.Peripheral.ETH.Descriptor.Types
import Ivory.BSP.STM32.Peripheral.ETH.Peripheral
import Ivory.BSP.STM32.Peripheral.ETH.Pins
import Ivory.BSP.STM32.Peripheral.ETH.Phy

data ETHConfig = ETHConfig
  { ethConfigPeriph :: ETH
  , ethConfigPins   :: ETHPins
  }
