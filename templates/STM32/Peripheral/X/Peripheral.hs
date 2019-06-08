{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
--
-- Peripheral.hs --- {{ type }} Peripheral Description
-- {{ type }} Peripheral type and constructor
--

module {{ modns }} where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig
--import Ivory.BSP.STM32.Peripheral.GPIOF4

--import Ivory.BSP.STM32.Peripheral.UARTPins
--import Ivory.BSP.STM32.Peripheral.UART1.Types
--import Ivory.BSP.STM32.Peripheral.UART1.Regs

data {{ type }} = {{ type }}
{{ bitDataRegs }}
  }

mk{{ type }} :: Integer -> {{ type }}
mk{{ type }} base  = {{ type }}
{{ bitDataRegsMk }}
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)
