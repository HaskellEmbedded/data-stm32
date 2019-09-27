
module Ivory.BSP.STM32.Peripheral.I2C.Pins (I2CPins(..)) where

import Ivory.BSP.STM32.Peripheral.GPIO

data I2CPins =
  I2CPins
    { i2cpins_sda :: GPIOPin
    , i2cpins_scl :: GPIOPin
    }


