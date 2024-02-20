{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- General Purpose Timer (TIM2 to TIM5)  register types
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.GTIM.RegTypes where

import Ivory.Language

-- Compare Mode bit field definitions:
[ivory|
 bitdata CCMRMode :: Bits 3
   = ccmr_mode_frzn     as 0
   | ccmr_mode_chact    as 1
   | ccmr_mode_chinact  as 2
   | ccmr_mode_ocreftog as 3
   | ccmr_mode_ocreflo  as 4
   | ccmr_mode_ocrefhi  as 5
   | ccmr_mode_pwm1     as 6
   | ccmr_mode_pwm2     as 7
|]

-- Capture/Compare Selection bit field definitions:
[ivory|
 bitdata CCSMode :: Bits 2
   = ccs_mode_out   as 0
   | ccs_mode_in1   as 1
   | ccs_mode_in2   as 2
   | ccs_mode_intrc as 3
|]

[ivory|
 bitdata SlaveMode :: Bits 3
   = sms_mode_none     as 0
   | sms_mode_enc1     as 1
   | sms_mode_enc2     as 2
   | sms_mode_enc3     as 3
   | sms_mode_reset    as 4
   | sms_mode_gated    as 5
   | sms_mode_trigger  as 6
   | sms_mode_ext_clk  as 7
|]

