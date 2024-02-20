{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.Language

import Ivory.HW

import {{ init_modns }}.Regs

-- Convenience type synonyms
data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , afioRCCEnable       :: forall eff . Ivory eff ()
  , afioRCCDisable      :: forall eff . Ivory eff ()
  }

-- | Create AFIO given the base register address.
mk{{ type }}
  :: Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> {{ type }}
mk{{ type }} base rccen rccdis = {{ type }}
{{{ bitDataRegsMk }}}
  , afioRCCEnable      = rccen
  , afioRCCDisable     = rccdis
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("afio->" ++ name)
