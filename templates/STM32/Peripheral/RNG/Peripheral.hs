{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.{{ type }}.Regs

-- Convenience type synonyms
data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , rngInterrupt       :: HasSTM32Interrupt
  , rngRCCEnable       :: forall eff . Ivory eff ()
  , rngRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create an RNG given the base register address.
mk{{ type }}
  :: (STM32Interrupt i)
  => Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> i
  -> {{ type }}
mk{{ type }} base rccen rccdis interrupt = {{ type }}
{{{ bitDataRegsMk }}}
  , rngInterrupt      = HasSTM32Interrupt interrupt
  , rngRCCEnable      = rccen
  , rngRCCDisable     = rccdis
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("rng->" ++ name)

