{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module {{ modns }}
  ( i2cTower
  , module Ivory.Tower.HAL.Bus.I2C
  , module Ivory.Tower.HAL.Bus.I2C.DeviceAddr
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.I2C

{{#versions}}
import qualified Ivory.BSP.STM32.Driver.I2Cv{{ version }} as V{{ version }}
{{/versions}}

i2cTower :: (e -> ClockConfig)
          -> I2C
          -> I2CPins
          -- -> Integer -- XXX: this should eat target I2C frequency but we're not there yet
          -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request")
                                            ('Struct "i2c_transaction_result")
                     , ChanOutput ('Stored ITime))
i2cTower tocc i2c pins = case i2c of
  {{#versions}}
  (WrappedV{{ version }} x) -> V{{ version }}.i2cTower tocc x pins
  {{/versions}}
