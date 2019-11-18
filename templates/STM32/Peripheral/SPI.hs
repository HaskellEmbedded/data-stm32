{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- due to unused versions
--
-- SPI.hs --- SPI peripheral
--
-- We generate multiple SPI peripherals but only SPIv2
-- which is compatible across all MCUs
--
-- The rest can be used for I2S

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


import Ivory.BSP.STM32.Peripheral.GPIO (GPIOPin, GPIO_AF)
import Ivory.BSP.STM32.Peripheral.SPI.Pins

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
       -> (GPIOPin -> GPIO_AF)
       -> String
       -> SPI
{{#versions}}
mkSPIVersion V{{ version }} i e1 e2 j c af s = WrappedV{{ version }} $ P{{ version }}.mkSPI i e1 e2 j c af s
{{/versions}}
--}
