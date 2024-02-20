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
  }

-- | Create an ETHMMC given the base register address.
mk{{ type }}
  :: Integer
  -> {{ type }}
mk{{ type }} base = {{ type }}
{{{ bitDataRegsMk }}}
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("ethmmc->" ++ name)
