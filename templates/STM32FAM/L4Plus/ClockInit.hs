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

init_clocks :: ClockConfig -> Def('[]':->())
init_clocks clockconfig = proc "init_clocks" $ body $ do
  comment ("platformClockConfig: " ++ (show cc)      ++ "\n" ++
           "sysclk: "  ++ (show (clockSysClkHz cc))  ++ "\n" ++
           "hclk:   "  ++ (show (clockHClkHz cc))    ++ "\n" ++
           "pclk1:  "  ++ (show (clockPClk1Hz cc))   ++ "\n" ++
           "pclk2:  "  ++ (show (clockPClk2Hz cc)))

  -- RCC clock config to default reset state
  -- we use MSI during init
  modifyReg rcc_reg_cr $ setBit rcc_cr_msion

  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_ppre2    rcc_pprex_none
    setField rcc_cfgr_ppre1    rcc_pprex_none
    setField rcc_cfgr_hpre     rcc_hpre_none
    setField rcc_cfgr_sw       rcc_sysclk_l4_msi

  -- Reset HSEOn, CSSOn, PLLOn bits
  modifyReg rcc_reg_cr $ do
    clearBit rcc_cr_hseon
    clearBit rcc_cr_csson
    clearBit rcc_cr_pllon

  -- Reset HSEBYP bit
  modifyReg rcc_reg_cr $ clearBit rcc_cr_hsebyp

  -- Disable all interrupts
  modifyReg rcc_reg_cir $ do
    clearBit rcc_cir_hsi48rdyie
    clearBit rcc_cir_lsecssie
    clearBit rcc_cir_pllsai2rdyie
    clearBit rcc_cir_pllsai1rdyie
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
        setField rcc_pllcfgr_pllsrc rcc_pllsrc_l4_hsi16
    MSI freq -> do
      modifyReg rcc_reg_cr $ do
        setBit rcc_cr_msion

      modifyReg rcc_reg_cr $ do
        setField rcc_cr_msirange (freqToMSIRange freq)
        -- MSI range is provided by MSIRANGE
        setBit rcc_cr_msirgsel

      forever $ do
        cr <- getReg rcc_reg_cr
        when (bitToBool (cr #. rcc_cr_msirdy)) $ breakOut

      modifyReg rcc_reg_pllcfgr $ do
        setField rcc_pllcfgr_pllsrc rcc_pllsrc_l4_msi
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
        modifyReg rcc_reg_pllcfgr $ do
          setField rcc_pllcfgr_pllsrc rcc_pllsrc_l4_hse

      -- Handle exception case when HSERDY fails.
      unless success $ do
        comment "waiting for HSERDY failed: check your hardware for a fault"
        comment "XXX handle this exception case with a breakpoint or reconfigure pll values for hsi"
        assert success
        forever $ return ()

    invalidSource -> error $ "Invalid clock source " ++ (show invalidSource)

  -- Select regulator voltage output scale 1 hi-perf mode (default is scale 2 mod, low power mode)
  modifyReg rcc_reg_apb1enr1 $ setBit rcc_apb1enr1_pwren
  modifyReg pwr_reg_cr1 $ setField pwr_cr1_vos (fromRep 1)

   -- Wait for voltage regulator to switch modes
  forever $ do
    sr <- getReg pwr_reg_sr2
    when (bitToBool (sr #. pwr_sr2_vosf) ==? false) $ breakOut

  -- Configure flash prefetch, instruction cache, data cache, 4 wait states
  modifyReg flash_reg_acr $ do
    setBit flash_acr_prften
    setBit flash_acr_icen
    setBit flash_acr_dcen
    setField flash_acr_latency (fromRep 4)

  -- Select bus clock dividers
  modifyReg rcc_reg_cfgr $ do
    setField rcc_cfgr_hpre  hpre_divider
    setField rcc_cfgr_ppre1 ppre1_divider
    setField rcc_cfgr_ppre2 ppre2_divider

  -- Configure main PLL:
  modifyReg rcc_reg_pllcfgr $ do
    setField rcc_pllcfgr_pllm m
    setField rcc_pllcfgr_plln n
    setField rcc_pllcfgr_pllr r
    setField rcc_pllcfgr_pllp (boolToBit p)
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
    setField rcc_cfgr_sw rcc_sysclk_l4_pll

  -- Spin until main PLL is ready:
  forever $ do
    cfgr <- getReg rcc_reg_cfgr
    when ((cfgr #. rcc_cfgr_sws) ==? rcc_sysclk_l4_pll) $ breakOut

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

  p = case pll_mnr_p (clockconfig_pll cc) of
        7 -> false
        17 -> true
        _ -> error  "platformClockConfig pll_mnr_p not in valid range, only 7 and 17 are valid"

  q = case pll_mnr_q (clockconfig_pll cc) of
        2 -> rcc_pllqr_l4_div2
        4 -> rcc_pllqr_l4_div4
        6 -> rcc_pllqr_l4_div6
        8 -> rcc_pllqr_l4_div8
        _ -> error "platformClockConfig pll_mnr_q not in valid range"

  r = case pll_mnr_r (clockconfig_pll cc) of
        2 -> rcc_pllqr_l4_div2
        4 -> rcc_pllqr_l4_div4
        6 -> rcc_pllqr_l4_div6
        8 -> rcc_pllqr_l4_div8
        _ -> error "platformClockConfig pll_mnr_r not in valid range"

  freqToMSIRange freq = case freq of
    100000   -> rcc_msirange_100khz
    200000   -> rcc_msirange_200khz
    400000   -> rcc_msirange_400khz
    800000   -> rcc_msirange_800khz
    1000000  -> rcc_msirange_1mhz
    2000000  -> rcc_msirange_2mhz
    4000000  -> rcc_msirange_4mhz
    8000000  -> rcc_msirange_8mhz
    16000000 -> rcc_msirange_16mhz
    24000000 -> rcc_msirange_24mhz
    32000000 -> rcc_msirange_32mhz
    48000000 -> rcc_msirange_48mhz
    _        -> error $ "Invalid frequency for MSIRANGE " ++ show freq

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

