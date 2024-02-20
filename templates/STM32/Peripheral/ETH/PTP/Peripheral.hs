{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Peripheral.ETH.{{ type }}.Regs

data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , ethPTPRCCEnable       :: forall eff . Ivory eff ()
  , ethPTPRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create an ETHPTP given the base register address.
mk{{ type }}
  :: Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> {{ type }}
mk{{ type }} base rccen rccdis = {{ type }}
{{{ bitDataRegsMk }}}
  , ethPTPRCCEnable      = rccen
  , ethPTPRCCDisable     = rccdis
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("ethptp->" ++ name)
