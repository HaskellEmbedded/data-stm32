{-# LANGUAGE Rank2Types #-}
--
-- SPI.hs --- SPI peripheral
--
-- Copyright (C) 2014, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.SPI
  ( P2.SPI(..)
  , P2.SPIDevice(..)
  , P2.mkSPI
  , P2.SPICSActive(..)
  , P2.SPIClockPolarity(..)
  , P2.SPIClockPhase(..)
  , P2.SPIBitOrder(..)
  , SPIPins(..)
  ) where

import Ivory.Language
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.SPI.Pins
import Ivory.BSP.STM32.Peripheral.SPI.RegTypes

{{#versions}}
import qualified Ivory.BSP.STM32.Peripheral.SPIv{{ version }}.Peripheral as P{{ version }}
{{/versions}}

{--
data SPIVersion =
{{#versions}}
  {{ prefix }} V{{ version }}
{{/versions}}

data SPI =
{{#versions}}
  {{ prefix }} WrappedV{{ version }} P{{ version }}.SPI
{{/versions}}

mkSPIVersion
       :: (STM32Interrupt i)
       => SPIVersion
       -> Integer
       -> (forall eff . Ivory eff ())
       -> (forall eff . Ivory eff ())
       -> i
       -> PClk
       -> String
       -> SPI
{{#versions}}
mkSPIVersion V{{ version }} i e1 e2 j c s = WrappedV{{ version }} $ P{{ version }}.mkSPI i e1 e2 j c s
{{/versions}}
--}
