{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module {{ modns }}
  ( init_clocks
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig
{-
import Ivory.BSP.STM32.Peripheral.Flash
import Ivory.BSP.STM32.Peripheral.PWR
-}
import Ivory.BSP.STM32.Peripheral.RCC.RegTypes

import Ivory.BSP.STM32{{ fam }}.FLASH
import Ivory.BSP.STM32{{ fam }}.PWR
import Ivory.BSP.STM32{{ fam }}.RCC

init_clocks :: ClockConfig -> Def('[]':->())
init_clocks clockconfig = proc "init_clocks" $ body $ do
  comment ("platformClockConfig: " ++ (show cc)      ++ "\n" ++
           "sysclk: "  ++ (show (clockSysClkHz cc))  ++ "\n" ++
           "hclk:   "  ++ (show (clockHClkHz cc))    ++ "\n" ++
           "pclk1:  "  ++ (show (clockPClk1Hz cc))   ++ "\n" ++
           "pclk2:  "  ++ (show (clockPClk2Hz cc)))

  -- RCC clock config to default reset state
  modifyReg rcc_reg_cr $ setBit rcc_cr_hsion
  modifyReg rcc_reg_cfgr $ do
    -- XXX: bits 3
    -- setField rcc_cfgr_mco      rcc_mcox_sysclk
    setField rcc_cfgr_ppre2    rcc_pprex_none
    setField rcc_cfgr_ppre1    rcc_pprex_none
    setField rcc_cfgr_hpre     rcc_hpre_none
    setField rcc_cfgr_sw       rcc_sysclk_hsi
    clearBit rcc_cfgr_pllsrc  -- use HSI

  -- Reset HSEOn, CSSOn, PLLOn bits
  modifyReg rcc_reg_cr $ do
    clearBit rcc_cr_hseon
    clearBit rcc_cr_csson
    clearBit rcc_cr_pllon

  -- Reset HSEBYP bit
  modifyReg rcc_reg_cr $ clearBit rcc_cr_hsebyp

  -- Disable all interrupts
  modifyReg rcc_reg_cir $ do
    --clearBit rcc_cir_plli2s_rdyie
    clearBit rcc_cir_pllrdyie
    clearBit rcc_cir_hserdyie
    clearBit rcc_cir_hsirdyie
    clearBit rcc_cir_lserdyie
    clearBit rcc_cir_lsirdyie
  case clockconfig_source cc of
    HSI8 -> return ()
    HSE _ -> do
      -- Enable HSE
      modifyReg rcc_reg_cr $ setBit rcc_cr_hseon

      -- Spin for a little bit waiting for RCC->CR HSERDY bit to be high
      hserdy <- local (ival false)
      arrayMap $ \(_ :: Ix 10240) -> do
        cr <- getReg rcc_reg_cr
        when (bitToBool (cr #. rcc_cr_hserdy)) $ do
          store hserdy true
          breakOut


      success <- deref hserdy
      when success $ do
        -- Set PLL to use external clock:
        modifyReg rcc_reg_cfgr $ do
          setBit rcc_cfgr_pllsrc -- use HSE

      -- Handle exception case when HSERDY fails.
      unless success $ do
        comment "waiting for HSERDY failed: check your hardware for a fault"
        comment "XXX handle this exception case with a breakpoint or reconfigure pll values for hsi"
        assert success
        forever $ return ()

    invalidSource -> error $ "Invalid clock source " ++ (show invalidSource)

  -- Select regulator voltage output scale 1 mode
  modifyReg rcc_reg_apb1enr $ setBit rcc_apb1enr_pwren
  --modifyReg pwr_reg_cr $ setBit pwr_cr_vos

  -- Select bus clock dividers
  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_hpre  hpre_divider
    setField rcc_cfgr_ppre1 ppre1_divider
    setField rcc_cfgr_ppre2 ppre2_divider

  -- Configure main PLL:
  modifyReg rcc_reg_cfgr $ do
    --setBit rcc_cfgr_pllsrc
    setField rcc_cfgr_pllxtpre m -- div
    setField rcc_cfgr_pllmul n   -- mul

  -- Enable main PLL:
  modifyReg rcc_reg_cr $ setBit rcc_cr_pllon
  -- Spin until RCC->CR PLLRDY bit is high
  forever $ do
    cr <- getReg rcc_reg_cr
    when (bitToBool (cr #. rcc_cr_pllrdy)) $ breakOut

  -- Configure flash prefetch, instruction cache, data cache, wait state 5
  modifyReg flash_reg_acr $ do
    setBit flash_acr_prftbe
    setField flash_acr_latency (fromRep 1)

  -- Select main PLL as system clock source
  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_sw rcc_sysclk_pll

  -- Spin until main PLL is ready:
  forever $ do
    cfgr <- getReg rcc_reg_cfgr
    when ((cfgr #. rcc_cfgr_sws) ==? rcc_sysclk_pll) $ breakOut

  where
  cc = clockconfig
  --cc = if clockPLL48ClkHz clockconfig == 48 * 1000 * 1000
  --        then clockconfig
  --        else error "ClockConfig invalid: 48MHz peripheral clock is wrong speed"
  mm = pll_m (clockconfig_pll cc)
  m = if mm > 1 && mm < 64
         then fromRep (fromIntegral mm)
         else error "platformClockConfig pll_m not in valid range"
  nn = pll_n (clockconfig_pll cc)
  n = if nn > 191 && nn < 433
         then fromRep (fromIntegral nn)
         else error "platformClockConfig pll_n not in valid range"
  {-
  p = case pll_p (clockconfig_pll cc) of
        2 -> rcc_pllp_div2
        4 -> rcc_pllp_div4
        6 -> rcc_pllp_div6
        8 -> rcc_pllp_div8
        _ -> error "platformClockConfig pll_p not in valid range"
  qq = pll_q (clockconfig_pll cc)
  q = if qq > 1 && qq < 16
         then fromRep (fromIntegral qq)
         else error "platformClockConfig pll_q not in valid range"
  -}
  hpre_divider = case clockconfig_hclk_divider cc of
    1   -> rcc_hpre_none
    2   -> rcc_hpre_div2
    4   -> rcc_hpre_div4
    8   -> rcc_hpre_div8
    16  -> rcc_hpre_div16
    64  -> rcc_hpre_div64
    128 -> rcc_hpre_div128
    256 -> rcc_hpre_div256
    512 -> rcc_hpre_div512
    _   -> error "platfomClockConfig hclk divider not in valid range"

  ppre1_divider = case clockconfig_pclk1_divider cc of
    1  -> rcc_pprex_none
    2  -> rcc_pprex_div2
    4  -> rcc_pprex_div4
    8  -> rcc_pprex_div8
    16 -> rcc_pprex_div16
    _  -> error "platformClockConfig pclk1 divider not in valid range"

  ppre2_divider = case clockconfig_pclk2_divider cc of
    1  -> rcc_pprex_none
    2  -> rcc_pprex_div2
    4  -> rcc_pprex_div4
    8  -> rcc_pprex_div8
    16 -> rcc_pprex_div16
    _  -> error "platformClockConfig pclk2 divider not in valid range"
  {-
  -}
