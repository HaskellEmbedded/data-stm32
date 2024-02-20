{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.ETH.{{ type }}.Regs

-- Convenience type synonyms
data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , ethInterrupt       :: HasSTM32Interrupt
  , ethRCCEnable       :: forall eff . Ivory eff ()
  , ethRCCDisable      :: forall eff . Ivory eff ()
  -- RX and TX clocks
  , ethRCCRXEnable     :: forall eff . Ivory eff ()
  , ethRCCRXDisable    :: forall eff . Ivory eff ()
  , ethRCCTXEnable     :: forall eff . Ivory eff ()
  , ethRCCTXDisable    :: forall eff . Ivory eff ()
  }

-- | Create an ETHMAC given the base register address.
mk{{ type }}
  :: (STM32Interrupt i)
  => Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> i
  -> {{ type }}
mk{{ type }} base rccen rccdis rcctxen rcctxdis rccrxen rccrxdis interrupt = {{ type }}
{{{ bitDataRegsMk }}}
  , ethInterrupt      = HasSTM32Interrupt interrupt
  , ethRCCEnable      = rccen
  , ethRCCDisable     = rccdis
  , ethRCCTXEnable    = rcctxen
  , ethRCCTXDisable   = rcctxdis
  , ethRCCRXEnable    = rccrxen
  , ethRCCRXDisable   = rccrxdis
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("ethmac->" ++ name)
