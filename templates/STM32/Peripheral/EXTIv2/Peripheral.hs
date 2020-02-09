{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.EXTI.RegTypes
import Ivory.BSP.STM32.Peripheral.{{ type }}{{ version }}.Regs

-- Convenience type synonyms
data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , extiInterrupts :: [(Int, Int, HasSTM32Interrupt)]
  , extiEnable     :: forall eff . Ivory eff ()
  , extiDisable    :: forall eff . Ivory eff ()
  , extiCR1        :: BitDataReg (EXTI_EXTICR)
  , extiCR2        :: BitDataReg (EXTI_EXTICR)
  , extiCR3        :: BitDataReg (EXTI_EXTICR)
  , extiCR4        :: BitDataReg (EXTI_EXTICR)
  }

-- set extiCRx to internal registers via this fixed point function
fix :: EXTI -> EXTI
fix e = e
  { extiCR1 = cvtReg $ extiRegEXTICR1 e
  , extiCR2 = cvtReg $ extiRegEXTICR2 e
  , extiCR3 = cvtReg $ extiRegEXTICR3 e
  , extiCR4 = cvtReg $ extiRegEXTICR4 e
  }

-- | Create an EXTI given the base register address.
mk{{ type }}  :: (STM32Interrupt i)
       => Integer
       -> (forall eff . Ivory eff ())
       -> (forall eff . Ivory eff ())
       -> BitDataReg (EXTI_EXTICR)
       -> BitDataReg (EXTI_EXTICR)
       -> BitDataReg (EXTI_EXTICR)
       -> BitDataReg (EXTI_EXTICR)
       -> [(Int, Int, i)]
       -> {{ type }}
mk{{ type }} base syscfgrccen syscfgrccdis r1 r2 r3 r4 isrs = fix $ {{ type }}
{{ bitDataRegsMk }}
    , extiInterrupts = map (\(s, e, i) -> (s, e, HasSTM32Interrupt i)) isrs
    , extiEnable = syscfgrccen
    , extiDisable = syscfgrccdis
    , extiCR1 = r1
    , extiCR2 = r2
    , extiCR3 = r3
    , extiCR4 = r4
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("exti->" ++ name)

