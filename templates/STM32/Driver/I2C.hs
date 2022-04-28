{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module {{ modns }}
  ( i2cTower
  , i2cTowerFrequency
  , i2cTowerStandard
  , i2cTowerFast
  , i2cTowerFastPlus
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

i2cTowerFrequency :: (e -> ClockConfig)
                  -> I2C
                  -> I2CPins
                  -> Integer -- target I2C frequency
                  -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request")
                                                    ('Struct "i2c_transaction_result")
                             , ChanOutput ('Stored ITime))
i2cTowerFrequency tocc i2c pins freq = case i2c of
  {{#versions}}
    (WrappedV{{ version }} x) -> V{{ version }}.i2cTower tocc x pins freq
  {{/versions}}

-- 400khz I2C (for compatibility)
i2cTower :: (e -> ClockConfig)
         -> I2C
         -> I2CPins
         -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request")
                                           ('Struct "i2c_transaction_result")
                    , ChanOutput ('Stored ITime))
i2cTower tocc i2c pins = i2cTowerFrequency tocc i2c pins 400000

-- 100khz standard I2C
i2cTowerStandard :: (e -> ClockConfig)
                 -> I2C
                 -> I2CPins
                 -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request")
                                                   ('Struct "i2c_transaction_result")
                            , ChanOutput ('Stored ITime))
i2cTowerStandard tocc i2c pins = i2cTowerFrequency tocc i2c pins 100000

-- 400khz fast I2C
i2cTowerFast :: (e -> ClockConfig)
             -> I2C
             -> I2CPins
             -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request")
                                               ('Struct "i2c_transaction_result")
                        , ChanOutput ('Stored ITime))
i2cTowerFast tocc i2c pins = i2cTowerFrequency tocc i2c pins 400000

-- 1Mhz fast mode+ I2C
i2cTowerFastPlus :: (e -> ClockConfig)
                 -> I2C
                 -> I2CPins
                 -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request")
                                                   ('Struct "i2c_transaction_result")
                            , ChanOutput ('Stored ITime))
i2cTowerFastPlus tocc i2c pins = i2cTowerFrequency tocc i2c pins 1000000
