{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.BSP.STM32.Peripheral.ETH.MAC.RegTypes where

import Ivory.Language

[ivory|
  -- MACCR.IFG
  bitdata InterframeGap :: Bits 3
    = inteframe_gap96 as 0b000
    | inteframe_gap88 as 0b001
    | inteframe_gap80 as 0b010
    | inteframe_gap72 as 0b011
    | inteframe_gap64 as 0b100
    | inteframe_gap56 as 0b101
    | inteframe_gap48 as 0b110
    | inteframe_gap40 as 0b111

  -- MACCR.BL
  bitdata BackoffLimit :: Bits 2
    = backoff_limit_10 as 0b00 -- For retransmission n, wait up to 2^min(n, 10) time slots
    | backoff_limit_8  as 0b01 -- For retransmission n, wait up to 2^min(n, 8) time slots
    | backoff_limit_4  as 0b10 -- For retransmission n, wait up to 2^min(n, 4) time slots
    | backoff_limit_1  as 0b11 -- For retransmission n, wait up to 2^min(n, 1) time slots

  -- MACMIIAR.CR
  bitdata ClockRange :: Bits 3
    = clock_range_hclk_div42  as 0b000 -- HCLK/42  60-100MHz
    | clock_range_hclk_div62  as 0b001 -- HCLK/62  100-150MHz
    | clock_range_hclk_div16  as 0b010 -- HCLK/16  20-35MHz
    | clock_range_hclk_div26  as 0b011 -- HCLK/26  35-60MHz
    | clock_range_hclk_div102 as 0b100 -- HCLK/102 150-216Mhz
|]

