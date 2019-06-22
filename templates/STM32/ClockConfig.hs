{-# LANGUAGE RecordWildCards #-}

module {{ modns }} where

import Ivory.Tower.Config

data ClockSource = External Integer | Internal deriving (Eq, Show)

data PLLFactor = PLLFactor
  { pll_m :: Integer -- divisor
  , pll_n :: Integer -- multiplier
  , pll_p :: Integer -- divisor for system clock
  , pll_q :: Integer -- divisor for 48MHz clocks
  }
  | PLLFactorF1
  { pll_mul :: Integer
  , pll_div :: Integer
  }
  | PLLFactorL4
  { pll_l4_m :: Integer -- divisor
  , pll_l4_n :: Integer -- multiplier
  , pll_l4_p :: Integer -- divisor for PLLP output
  , pll_l4_q :: Integer -- divisor for PLLQ output
  , pll_l4_r :: Integer -- divisor for system clock
  } deriving (Eq, Show)

data ClockConfig =
  ClockConfig
    { clockconfig_source        :: ClockSource
    , clockconfig_pll           :: PLLFactor
    , clockconfig_hclk_divider  :: Integer    -- HPRE
    , clockconfig_pclk1_divider :: Integer    -- PPRE1
    , clockconfig_pclk2_divider :: Integer    -- PPRE2
    } deriving (Eq, Show)

pllFactor :: PLLFactor -> Integer -> Integer
pllFactor PLLFactor{..}   x = (x `div` pll_m) * pll_n `div` pll_p
pllFactor PLLFactorF1{..} x = (x `div` pll_div) * pll_mul
pllFactor PLLFactorL4{..} x = (x `div` pll_l4_m) * pll_l4_n `div` pll_l4_r

clockSourceHz :: ClockSource -> Integer
clockSourceHz (External rate) = rate
clockSourceHz Internal        = 16 * 1000 * 1000

-- compute frequency of system clock from clock configuration
clockSysClkHz :: ClockConfig -> Integer
clockSysClkHz cc = pllFactor pll source
  where
  pll = clockconfig_pll cc
  source = clockSourceHz (clockconfig_source cc)

-- Hclk / AHB frequency, SysClk / hclk_divider (hpre)
clockHClkHz :: ClockConfig -> Integer
clockHClkHz cc = clockSysClkHz cc `div` (clockconfig_hclk_divider cc)

-- PClk1 / ABP1 frequency, HClk / Pclk1 (ppre1 divider)
clockPClk1Hz :: ClockConfig -> Integer
clockPClk1Hz cc = clockHClkHz cc `div` (clockconfig_pclk1_divider cc)

-- PClk2 / ABP2 frequency, HClk / Pclk2 (ppre2 divider)
clockPClk2Hz :: ClockConfig -> Integer
clockPClk2Hz cc = clockHClkHz cc `div` (clockconfig_pclk2_divider cc)

data PClk = PClk1 | PClk2

clockPClkHz :: PClk -> ClockConfig -> Integer
clockPClkHz PClk1 = clockPClk1Hz
clockPClkHz PClk2 = clockPClk2Hz

-- HSE
externalXtal :: Integer -> Integer -> ClockConfig
externalXtal xtal_mhz sysclk_mhz = ClockConfig
  { clockconfig_source = External (xtal_mhz * 1000 * 1000)
  , clockconfig_pll    = PLLFactor
      { pll_m = xtal_mhz
      , pll_n = sysclk_mhz * p
      , pll_p = p
      , pll_q = 7
      }
  , clockconfig_hclk_divider = 1
  , clockconfig_pclk1_divider = 4
  , clockconfig_pclk2_divider = 2
  }
  where p = 2

clockConfigParser :: ConfigParser ClockConfig
clockConfigParser = do
  xtal_mhz   <- subsection "xtalMHz" integer
  sysclk_mhz <- subsection "sysclkMHz" (integer `withDefault` 168)
  return (externalXtal xtal_mhz sysclk_mhz)
