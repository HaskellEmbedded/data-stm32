{-# LANGUAGE RecordWildCards #-}

module {{ modns }} (
  module Data.STM32.Clock
  , clockConfigParser
  ) where

import Data.STM32.Clock
import Ivory.Tower.Config

clockConfigParser :: ConfigParser ClockConfig
clockConfigParser = do
  xtal_mhz   <- subsection "xtalMHz" integer
  sysclk_mhz <- subsection "sysclkMHz" (integer `withDefault` 168)
  return (externalXtal xtal_mhz sysclk_mhz)
