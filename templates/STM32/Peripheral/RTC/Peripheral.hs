{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.Language

import Ivory.HW

--import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.{{ type }}.Regs

-- Convenience type synonyms
data {{ type }} = {{ type }}
{{ bitDataRegs }}
--  , rtcInterrupt       :: HasSTM32Interrupt
  , rtcRCCEnable       :: forall eff . Ivory eff ()
  , rtcRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create an RTC given the base register address.
--mk{{ type }}  :: (STM32Interrupt i)
--       => Integer
mk{{ type }}
  :: Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -- -> i
  -> {{ type }}
mk{{ type }} base rccen rccdis = {{ type }}
{{{ bitDataRegsMk }}}
--    , rtcInterrupt      = HasSTM32Interrupt interrupt
  , rtcRCCEnable      = rccen
  , rtcRCCDisable     = rccdis
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("rtc->" ++ name)

