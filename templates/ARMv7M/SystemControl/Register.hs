{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Registerhs --- ARMv7 SCR register.
--

module Ivory.BSP.ARMv7M.SystemControl.Register where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.ARMv7M.MemoryMap

----------------------------------------------------------------------
-- SCB->SCR (System Control Register)

[ivory|
  bitdata SCR :: Bits 32 = sbr_scr
    { _ :: Bits 27
    , scr_sendEventOnPending :: Bit -- SEVONPEND
    , _  :: Bit
    , scr_sleepDeep   :: Bit  -- Controls whether the processor uses sleep or deep sleep as its low power mode
    , scr_sleepOnExit :: Bit  -- Indicates sleep-on-exit when returning from Handler mode to Thread mode
    , _  :: Bit
    }
|]

scr_reg :: BitDataReg SCR
scr_reg = mkBitDataRegNamed system_control_register_base "scb_scr"

sbr_reg_scr :: BitDataReg SCR
sbr_reg_scr = scr_reg
