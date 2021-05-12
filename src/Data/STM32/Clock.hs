{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.STM32.Clock where

import GHC.Generics (Generic)
import Data.Serialize

data ClockSource =
    HSE Integer -- High speed external
  | HSI Integer -- High speed internal
  | MSI Integer -- Medium speed internal
  | LSI Integer -- Low speed internal
  | LSE Integer -- Low speed external
  deriving (Eq, Show, Ord, Generic)

instance Serialize ClockSource

clockSourceName :: ClockSource -> String
clockSourceName (HSE _) = "HSE"
clockSourceName (HSI _) = "HSI"
clockSourceName (MSI _) = "MSI"
clockSourceName (LSE _) = "LSE"
clockSourceName (LSI _) = "LSI"

clockSourceNames :: [ String ]
clockSourceNames = map clockSourceName [
    HSE 0
  , HSI 0
  , MSI 0
  , LSE 0
  , LSI 0
  ]

data PLLFactor =
    PLLFactorMNP         -- common for F4,F7 series
  { pll_m :: Integer     -- divisor
  , pll_n :: Integer     -- multiplier
  , pll_p :: Integer     -- divisor for system clock
  , pll_q :: Integer     -- divisor for 48MHz clocks
  }
  | PLLFactorMulDiv      -- F0,F1,F3 series
  { pll_mul :: Integer
  , pll_div :: Integer
  }
  | PLLFactorMNR         -- G0,G4,L4,L4+ series
  { pll_mnr_m :: Integer -- divisor
  , pll_mnr_n :: Integer -- multiplier
  , pll_mnr_p :: Integer -- divisor for PLLP output
  , pll_mnr_q :: Integer -- divisor for PLLQ output
  , pll_mnr_r :: Integer -- divisor for system clock
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
pllFactor PLLFactorMNP{..}   x = (x `div` pll_m) * pll_n `div` pll_p
pllFactor PLLFactorMulDiv{..} x = (x `div` pll_div) * pll_mul
pllFactor PLLFactorMNR{..} x = (x `div` pll_mnr_m) * pll_mnr_n `div` pll_mnr_r

clockSourceHz :: ClockSource -> Integer
clockSourceHz (HSE rate) = rate
clockSourceHz (HSI rate) = rate
clockSourceHz (MSI rate) = rate
clockSourceHz (LSE rate) = rate
clockSourceHz (LSI rate) = rate

-- compute frequency of system clock from clock configuration
clockSysClkHz :: ClockConfig -> Integer
clockSysClkHz cc = pllFactor pll source
  where
  pll = clockconfig_pll cc
  source = clockSourceHz (clockconfig_source cc)

-- Hclk / AHB frequency, SysClk / hclk_divider (hpre)
clockHClkHz :: ClockConfig -> Integer
clockHClkHz cc = clockSysClkHz cc `div` clockconfig_hclk_divider cc

-- PClk1 / ABP1 frequency, HClk / Pclk1 (ppre1 divider)
clockPClk1Hz :: ClockConfig -> Integer
clockPClk1Hz cc = clockHClkHz cc `div` clockconfig_pclk1_divider cc

-- PClk2 / ABP2 frequency, HClk / Pclk2 (ppre2 divider)
clockPClk2Hz :: ClockConfig -> Integer
clockPClk2Hz cc = clockHClkHz cc `div` clockconfig_pclk2_divider cc

data PClk = PClk1 | PClk2

clockPClkHz :: PClk -> ClockConfig -> Integer
clockPClkHz PClk1 = clockPClk1Hz
clockPClkHz PClk2 = clockPClk2Hz

-- HSE
-- F4 specific
externalXtalF4 :: Integer -> Integer -> ClockConfig
externalXtalF4 xtal_mhz sysclk_mhz = ClockConfig
  { clockconfig_source = HSE (xtal_mhz * 1000 * 1000)
  , clockconfig_pll    = PLLFactorMNP
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
