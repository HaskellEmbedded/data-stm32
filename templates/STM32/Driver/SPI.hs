{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- due to unused versions

module {{ modns }}
  ( V2.spiTower
  , module Ivory.Tower.HAL.Bus.SPI
  , module Ivory.Tower.HAL.Bus.SPI.DeviceHandle
  ) where

import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

{{#versions}}
import qualified Ivory.BSP.STM32.Driver.SPIv{{ version }} as V{{ version }}
{{/versions}}

{--
spiTower :: forall e . (e -> ClockConfig)
          -> SPI
          -> [SPIDevice]
          -> Tower e ( BackpressureTransmit ('Struct "spi_transaction_request")
                                            ('Struct "spi_transaction_result")
                     , ChanOutput ('Stored ITime))
spiTower tocc spi pins baud = case spi of
  {{#versions}}
  (WrappedV{{ version }} x) -> V{{ version }}.spiTower tocc x pins baud
  {{/versions}}
--}
