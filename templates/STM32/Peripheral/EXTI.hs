{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Ivory.BSP.STM32.Peripheral.EXTI
  ( mkEXTIVersion
  , EXTI(..)
  , EXTIVersion(..)
  , module Ivory.BSP.STM32.Peripheral.EXTI.RegTypes
  , module Ivory.BSP.STM32.Peripheral.EXTI.Types
  ) where

import Ivory.Language
import Ivory.HW
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.EXTI.RegTypes
import Ivory.BSP.STM32.Peripheral.EXTI.Types

{{#versions}}
import qualified Ivory.BSP.STM32.Peripheral.EXTIv{{ version }}.Peripheral as P{{ version }}
{{/versions}}

data EXTIVersion =
{{#versions}}
  {{ prefix }} V{{ version }}
{{/versions}}

data EXTI =
{{#versions}}
  {{ prefix }} WrappedV{{ version }} P{{ version }}.EXTI
{{/versions}}

mkEXTIVersion :: (STM32Interrupt i)
       => EXTIVersion
       -> Integer
       -> (forall eff . Ivory eff ())
       -> (forall eff . Ivory eff ())
       -> BitDataReg (EXTI_EXTICR)
       -> BitDataReg (EXTI_EXTICR)
       -> BitDataReg (EXTI_EXTICR)
       -> BitDataReg (EXTI_EXTICR)
       -> [(Int, Int, i)]
       -> EXTI
{{#versions}}
mkEXTIVersion V{{ version }} base syscfgrccen syscfgrccdis r1 r2 r3 r4 isrs = WrappedV{{ version }} $ P{{ version }}.mkEXTI base syscfgrccen syscfgrccdis r1 r2 r3 r4 isrs
{{/versions}}

