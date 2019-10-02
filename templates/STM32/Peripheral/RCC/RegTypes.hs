{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
--
-- RegTypes.hs --- Types for register fields in RCC driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.RCC.RegTypes where

import Ivory.Language

[ivory|
 bitdata RCC_PLLP     :: Bits 2
 = rcc_pllp_div2      as 0
 | rcc_pllp_div4      as 1
 | rcc_pllp_div6      as 2
 | rcc_pllp_div8      as 3

 bitdata RCC_MCOx     :: Bits 2
 = rcc_mcox_sysclk    as 0
 | rcc_mcox_plli2s    as 1
 | rcc_mcox_hse       as 2
 | rcc_mcox_pll       as 3

 bitdata RCC_MCOxPre  :: Bits 3
  = rcc_mcoxpre_none  as 0 -- 0b0xx
  | rcc_mcoxpre_div2  as 4 -- 0b100
  | rcc_mcoxpre_div3  as 5 -- 0b101
  | rcc_mcoxpre_div4  as 6 -- 0b110
  | rcc_mcoxpre_div5  as 7 -- 0b111

 bitdata RCC_PPREx    :: Bits 3
  = rcc_pprex_none    as 0 -- 0b0xx
  | rcc_pprex_div2    as 4 -- 0b100
  | rcc_pprex_div4    as 5 -- 0b100
  | rcc_pprex_div8    as 6 -- 0b100
  | rcc_pprex_div16   as 7 -- 0b111

 bitdata RCC_HPRE     :: Bits 4
  = rcc_hpre_none     as 0  -- 0b0xxx
  | rcc_hpre_div2     as 8  -- 0b1000
  | rcc_hpre_div4     as 9  -- 0b1001
  | rcc_hpre_div8     as 10 -- 0b1010
  | rcc_hpre_div16    as 11 -- 0b1011
  | rcc_hpre_div64    as 12 -- 0b1100
  | rcc_hpre_div128   as 13 -- 0b1101
  | rcc_hpre_div256   as 14 -- 0b1110
  | rcc_hpre_div512   as 15 -- 0b1111

 bitdata RCC_SYSCLK   :: Bits 2
  = rcc_sysclk_hsi    as 0
  | rcc_sysclk_hse    as 1
  | rcc_sysclk_pll    as 2

 -- F0
 bitdata RCC_SYSCLK_F0   :: Bits 2
  = rcc_sysclk_f0_hsi   as 0
  | rcc_sysclk_f0_hse   as 1
  | rcc_sysclk_f0_pll   as 2
  | rcc_sysclk_f0_hsi48 as 3

 bitdata RCC_PLLSRC_F0   :: Bits 2
  = rcc_pllsrc_f0_hsi_div_2        as 0
  | rcc_pllsrc_f0_hsi_div_prediv   as 1
  | rcc_pllsrc_f0_hse_div_prediv   as 2
  | rcc_pllsrc_f0_hsi48_div_prediv as 3

 -- L4
 bitdata RCC_SYSCLK_L4   :: Bits 2
  = rcc_sysclk_l4_msi    as 0
  | rcc_sysclk_l4_hsi    as 1
  | rcc_sysclk_l4_hse    as 2
  | rcc_sysclk_l4_pll    as 3

 -- PLLQ & PLLR
 bitdata RCC_PLLQR_L4     :: Bits 2
 = rcc_pllqr_l4_div2      as 0
 | rcc_pllqr_l4_div4      as 1
 | rcc_pllqr_l4_div6      as 2
 | rcc_pllqr_l4_div8      as 3

 bitdata RCC_PLLSRC_L4   :: Bits 2
  = rcc_pllsrc_l4_off    as 0
  | rcc_pllsrc_l4_msi    as 1
  | rcc_pllsrc_l4_hsi16  as 2
  | rcc_pllsrc_l4_hse    as 3

 bitdata RCC_MSIRANGE_L4 :: Bits 4
  = rcc_msirange_100khz as 0
  | rcc_msirange_200khz as 1
  | rcc_msirange_400khz as 2
  | rcc_msirange_800khz as 3
  | rcc_msirange_1mhz   as 4
  | rcc_msirange_2mhz   as 5
  | rcc_msirange_4mhz   as 6 -- reset value
  | rcc_msirange_8mhz   as 7
  | rcc_msirange_16mhz  as 8
  | rcc_msirange_24mhz  as 9
  | rcc_msirange_32mhz  as 10
  | rcc_msirange_48mhz  as 11

 -- G0
 bitdata RCC_SYSCLK_G0 :: Bits 3
  = rcc_sysclk_g0_hsi16   as 0
  | rcc_sysclk_g0_hse     as 1
  | rcc_sysclk_g0_pll     as 2
  | rcc_sysclk_g0_lsi     as 3
  | rcc_sysclk_g0_lse     as 4

 bitdata RCC_PLLSRC_G0 :: Bits 2
  = rcc_pllsrc_g0_off   as 0
  | rcc_pllsrc_g0_hsi16 as 2
  | rcc_pllsrc_g0_hse   as 3

 -- G4
 bitdata RCC_SYSCLK_G4 :: Bits 2
  = rcc_sysclk_g4_hsi16   as 1
  | rcc_sysclk_g4_hse     as 2
  | rcc_sysclk_g4_pll     as 3

 bitdata RCC_PLLSRC_G4 :: Bits 2
  = rcc_pllsrc_g4_off   as 0
  | rcc_pllsrc_g4_hsi16 as 2
  | rcc_pllsrc_g4_hse   as 3

 -- PLLQ & PLLR
 bitdata RCC_PLLQR_G4     :: Bits 2
 = rcc_pllqr_g4_div2      as 0
 | rcc_pllqr_g4_div4      as 1
 | rcc_pllqr_g4_div6      as 2
 | rcc_pllqr_g4_div8      as 3

|]
