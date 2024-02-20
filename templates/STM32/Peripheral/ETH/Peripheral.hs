module Ivory.BSP.STM32.Peripheral.ETH.Peripheral
  ( ETH(..)
  , mkETH
  , module Ivory.BSP.STM32.Peripheral.ETH.DMA.Peripheral
  , module Ivory.BSP.STM32.Peripheral.ETH.MAC.Peripheral
  , module Ivory.BSP.STM32.Peripheral.ETH.MMC.Peripheral
  , module Ivory.BSP.STM32.Peripheral.ETH.PTP.Peripheral
  )
  where

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.ETH.DMA.Peripheral
import Ivory.BSP.STM32.Peripheral.ETH.MAC.Peripheral
import Ivory.BSP.STM32.Peripheral.ETH.MMC.Peripheral
import Ivory.BSP.STM32.Peripheral.ETH.PTP.Peripheral

-- | Grouped ETH peripherals
data ETH = ETH
  { ethDMA :: ETHDMA
  , ethMAC :: MAC
  , ethMMC :: MMC
  , ethPTP :: PTP
  , ethAFLookup :: GPIOPin -> GPIO_AF
  }

-- | Create grouped ETH peripheral
mkETH
  :: ETHDMA
  -> MAC
  -> MMC
  -> PTP
  -> (GPIOPin -> GPIO_AF) -- Alternate Function (AF) lookup
  -> ETH
mkETH = ETH
