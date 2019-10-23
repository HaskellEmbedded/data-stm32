{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.{{ type }}.Regs

-- Convenience type synonyms
data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , iwdgClock :: ClockSource
  }

-- | Create an IWDG given the base register address.
mk{{ type }}  :: Integer -> ClockSource -> {{ type }}
mk{{ type }} base clk = {{ type }}
{{ bitDataRegsMk }}
  , iwdgClock = clk
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) ("iwdg->" ++ name)

