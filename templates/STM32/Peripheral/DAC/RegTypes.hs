{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- DAC register types
--

module Ivory.BSP.STM32.Peripheral.DAC.RegTypes where

import Ivory.Language

[ivory|
  bitdata DACMAMP :: Bits 4
    = dac_mamp1    as 0b0000 -- Unmask bit0 of LFSR/ triangle amplitude equal to 1
    | dac_mamp3    as 0b0001 -- Unmask bits[1:0] of LFSR/ triangle amplitude equal to 3
    | dac_mamp7    as 0b0010 -- Unmask bits[2:0] of LFSR/ triangle amplitude equal to 7
    | dac_mamp15   as 0b0011 -- Unmask bits[3:0] of LFSR/ triangle amplitude equal to 15
    | dac_mamp31   as 0b0100 -- Unmask bits[4:0] of LFSR/ triangle amplitude equal to 31
    | dac_mamp63   as 0b0101 -- Unmask bits[5:0] of LFSR/ triangle amplitude equal to 63
    | dac_mamp127  as 0b0110 -- Unmask bits[6:0] of LFSR/ triangle amplitude equal to 127
    | dac_mamp255  as 0b0111 -- Unmask bits[7:0] of LFSR/ triangle amplitude equal to 255
    | dac_mamp511  as 0b1000 -- Unmask bits[8:0] of LFSR/ triangle amplitude equal to 511
    | dac_mamp1023 as 0b1001 -- Unmask bits[9:0] of LFSR/ triangle amplitude equal to 1023
    | dac_mamp2047 as 0b1010 -- Unmask bits[10:0] of LFSR/ triangle amplitude equal to 2047
    | dac_mamp4095 as 0b1011 -- Unmask bits[11:0] of LFSR/ triangle amplitude equal to 4095

  bitdata DACWAVE :: Bits 2
    = dac_wave_disable  as 0b00 -- Wave generation disabled
    | dac_wave_noise    as 0b01 -- Noise wave
    | dac_wave_triangle as 0b10 -- Triangle wave

  bitdata DACTSEL :: Bits 3
    = dac_tsel_tim6 as 0b000 -- Timer 6 TRGO event
    | dac_tsel_tim8 as 0b001 -- Timer 8 TRGO event
    | dac_tsel_tim7 as 0b010 -- Timer 7 TRGO event
    | dac_tsel_tim5 as 0b011 -- Timer 5 TRGO event
    | dac_tsel_tim2 as 0b100 -- Timer 2 TRGO event
    | dac_tsel_tim4 as 0b101 -- Timer 4 TRGO event
    | dac_tsel_ext  as 0b110 -- External line9
    | dac_tsel_sw   as 0b111 -- Software trigger
|]
