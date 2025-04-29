
module Ivory.BSP.STM32.Peripheral.ETH.Pins (ETHPins(..)) where

import Ivory.BSP.STM32.Peripheral.GPIO

data ETHPins = ETHPins
  { ethPins_mdc    :: GPIOPin
  , ethPins_mdio   :: GPIOPin
  , ethPins_refclk :: GPIOPin
  , ethPins_crs    :: GPIOPin
  , ethPins_txen   :: GPIOPin
  , ethPins_txd0   :: GPIOPin
  , ethPins_txd1   :: GPIOPin
  , ethPins_rxd0   :: GPIOPin
  , ethPins_rxd1   :: GPIOPin
  } deriving (Show)
