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
import Ivory.BSP.STM32.Peripheral.RCC.RegTypes

import Ivory.BSP.STM32{{ fam }}.FLASH
import Ivory.BSP.STM32{{ fam }}.PWR
import Ivory.BSP.STM32{{ fam }}.RCC

init_clocks :: ClockConfig -> Def('[]:->())
init_clocks clockconfig = proc "init_clocks" $ body $ do
  comment ("platformClockConfig: " ++ (show cc)      ++ "\n" ++
           "sysclk: "  ++ (show (clockSysClkHz cc))  ++ "\n" ++
           "hclk:   "  ++ (show (clockHClkHz cc))    ++ "\n" ++
           "pclk1:  "  ++ (show (clockPClk1Hz cc))   ++ "\n" ++
           "pclk2:  "  ++ (show (clockPClk2Hz cc)))

  -- RCC clock config to default reset state
  modifyReg rcc_reg_cr $ setBit rcc_cr_hsion

  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_ppre     rcc_pprex_none
    setField rcc_cfgr_hpre     rcc_hpre_none
    setField rcc_cfgr_sw       rcc_sysclk_g0_hsi16

  -- Reset HSEOn, CSSOn, PLLOn bits
  modifyReg rcc_reg_cr $ do
    clearBit rcc_cr_hseon
    clearBit rcc_cr_csson
    clearBit rcc_cr_pllon

  -- Reset HSEBYP bit
  modifyReg rcc_reg_cr $ clearBit rcc_cr_hsebyp

  -- Disable all interrupts
  modifyReg rcc_reg_cir $ do
    clearBit rcc_cir_pllrdyie
    clearBit rcc_cir_hserdyie
    clearBit rcc_cir_hsirdyie
    clearBit rcc_cir_lserdyie
    clearBit rcc_cir_lsirdyie

  case clockconfig_source cc of
    HSI _ -> do
      modifyReg rcc_reg_cr $ do
        setBit rcc_cr_hsion
      modifyReg rcc_reg_pllcfgr $ do
        setField rcc_pllcfgr_pllsrc rcc_pllsrc_g0_hsi16
    HSE _ -> do
      -- Enable HSE
      modifyReg rcc_reg_cr $ setBit rcc_cr_hseon

      -- Spin for a little bit waiting for RCC->CR HSERDY bit to be high
      hserdy <- local (ival false)
      arrayMap $ \(_ :: Ix 102400) -> do
        cr <- getReg rcc_reg_cr
        when (bitToBool (cr #. rcc_cr_hserdy)) $ do
          store hserdy true
          breakOut

      success <- deref hserdy
      when success $ do
        -- Set PLL to use external clock:
        modifyReg rcc_reg_pllcfgr $ do
          setField rcc_pllcfgr_pllsrc rcc_pllsrc_g0_hse

      -- Handle exception case when HSERDY fails.
      unless success $ do
        comment "waiting for HSERDY failed: check your hardware for a fault"
        comment "XXX handle this exception case with a breakpoint or reconfigure pll values for hsi"
        assert success
        forever $ return ()

    invalidSource -> error $ "Invalid clock source " ++ (show invalidSource)

  -- Select regulator voltage output scale 1 hi-perf mode (default is scale 2 mod, low power mode)
  modifyReg rcc_reg_apbenr1 $ setBit rcc_apbenr1_pwren
  modifyReg pwr_reg_cr1 $ setField pwr_cr1_vos (fromRep 1)

   -- Wait for voltage regulator to switch modes
  forever $ do
    sr <- getReg pwr_reg_sr2
    when (bitToBool (sr #. pwr_sr2_vosf) ==? false) $ breakOut

  -- Configure flash prefetch, instruction cache, data cache, 4 wait states
  modifyReg flash_reg_acr $ do
    setBit flash_acr_prften
    setBit flash_acr_icen
    setField flash_acr_latency (fromRep 4)

  -- Select bus clock dividers
  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_hpre hpre_divider
    setField rcc_cfgr_ppre ppre1_divider

  -- Configure main PLL:
  modifyReg rcc_reg_pllcfgr $ do
    setField rcc_pllcfgr_pllm m
    setField rcc_pllcfgr_plln n
    setField rcc_pllcfgr_pllr r
    setField rcc_pllcfgr_pllp p
    setField rcc_pllcfgr_pllq q
    -- Main PLL PLLCLK output enable
    setBit rcc_pllcfgr_pllren

  -- Enable main PLL:
  modifyReg rcc_reg_cr $ setBit rcc_cr_pllon
  -- Spin until RCC->CR PLLRDY bit is high
  forever $ do
    cr <- getReg rcc_reg_cr
    when (bitToBool (cr #. rcc_cr_pllrdy)) $ breakOut

  -- Select main PLL as system clock source
  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_sw rcc_sysclk_g0_pll

  -- Spin until main PLL is ready:
  forever $ do
    cfgr <- getReg rcc_reg_cfgr
    when ((cfgr #. rcc_cfgr_sws) ==? rcc_sysclk_g0_pll) $ breakOut

  where
  cc = clockconfig
  mm = pll_mnr_m (clockconfig_pll cc)
  m = if mm >= 1 && mm <= 8
         -- we substract 1 from PLLM to get
         -- 000: PLLM = 1
         -- 111: PLLM = 8
         then fromRep (fromIntegral $ mm - 1)
         else error "platformClockConfig pll_mnr_m not in valid range"

  nn = pll_mnr_n (clockconfig_pll cc)
  n = if nn >= 8 && nn <= 86
         then fromRep (fromIntegral nn)
         else error "platformClockConfig pll_mnr_n not in valid range"

  rr = pll_mnr_r (clockconfig_pll cc)
  r = if rr >= 2 && rr <= 8
         -- we substract 1 from PLLR to get
         -- 001: PLLR = 2
         -- 111: PLLR = 8
         then fromRep (fromIntegral rr - 1)
         else error "platformClockConfig pll_mnr_r not in valid range (2-8 inclusive)"

  pp = pll_mnr_p (clockconfig_pll cc)
  p = if pp >= 2 && pp <= 32
         -- we substract 1 from PLLP to get
         -- 0001: PLLQ = 2
         -- 1111: PLLQ = 32
         then fromRep (fromIntegral pp - 1)
         else error "platformClockConfig pll_mnr_p not in valid range (2-32 inclusive)"

  qq = pll_mnr_q (clockconfig_pll cc)
  q = if qq >= 2 && qq <= 8
         -- we substract 1 from PLLQ to get
         -- 001: PLLQ = 2
         -- 111: PLLQ = 8
         then fromRep (fromIntegral qq - 1)
         else error "platformClockConfig pll_mnr_q not in valid range (2-8 inclusive)"

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
