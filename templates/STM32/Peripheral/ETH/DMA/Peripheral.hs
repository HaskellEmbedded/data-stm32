{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Peripheral.ETH.{{ type }}.Regs

data ETH{{ type }} = ETH{{ type }}
{{ bitDataRegs }}
  }

-- | Create an ETHDMA given the base register address.
mkETH{{ type }}
  :: Integer
  -> ETH{{ type }}
mkETH{{ type }} base = ETH{{ type }}
{{{ bitDataRegsMk }}}
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("ethdma->" ++ name)
