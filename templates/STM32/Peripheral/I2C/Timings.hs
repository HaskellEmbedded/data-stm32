{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Ivory.BSP.STM32.Peripheral.I2C.Timings where

-- only used by I2Cv2

import Data.List

nsPerSec :: Int
nsPerSec = 1000_000_000

-- all times in nanoseconds
analogFilterDelayMin :: Nanoseconds
analogFilterDelayMin = 50

analogFilterDelayMax :: Nanoseconds
analogFilterDelayMax = 260

defaultRiseTime :: Int
defaultRiseTime = 25
defaultFallTime :: Int
defaultFallTime = 10

--fastThreshold = 250_000
--fastPlusThreshold = 700_000

type Hz = Int
type Nanoseconds = Int

data I2CTimingSpecs = I2CTimingSpecs {
    rateMin :: Hz
  , rateMax :: Hz
  , fallMax :: Nanoseconds
  , riseMax :: Nanoseconds
  , hddatMin :: Nanoseconds -- tHD;DAT Data hold time
  , vddatMax :: Nanoseconds -- tVD;DAT Data valid time
  , sudatMin :: Nanoseconds -- tSU;DAT Data setup time
  , lMin :: Nanoseconds --  Min low period of the SCL clock
  , hMin :: Nanoseconds --  Min high period of the SCL clock
  }

i2cTimingStandard :: I2CTimingSpecs
i2cTimingStandard = I2CTimingSpecs {
    rateMin = 80_000
  , rateMax = 100_000
  , fallMax = 300
  , riseMax = 1000
  , hddatMin = 0
  , vddatMax = 3450
  , sudatMin = 250
  , lMin = 4700
  , hMin = 4000
  }

i2cTimingFast :: I2CTimingSpecs
i2cTimingFast = I2CTimingSpecs {
    rateMin = 320_000
  , rateMax = 400_000
  , fallMax = 300
  , riseMax = 300
  , hddatMin = 0
  , vddatMax = 900
  , sudatMin = 100
  , lMin = 1300
  , hMin = 600
  }

i2cTimingFastPlus :: I2CTimingSpecs
i2cTimingFastPlus = I2CTimingSpecs {
    rateMin = 800_000
  , rateMax = 1_000_000
  , fallMax = 120
  , riseMax = 120
  , hddatMin = 0
  , vddatMax = 450
  , sudatMin = 50
  , lMin = 500
  , hMin = 260
  }

i2cTimingSMBus :: I2CTimingSpecs
i2cTimingSMBus = I2CTimingSpecs {
    rateMin = 100_000
  , rateMax = 100_000
  , fallMax = 300
  , riseMax = 1000
  , hddatMin = 300
  , vddatMax = 0 -- actually undef for smbus
  , sudatMin = 250
  , lMin = 4700
  , hMin = 4000
  }

allI2CTimings :: [I2CTimingSpecs]
allI2CTimings = [
    i2cTimingStandard
  , i2cTimingFast
  , i2cTimingFastPlus
  ]

data I2CTiming = I2CTiming {
    timingError     :: Int
  , timingPrescaler :: Int
  , timingSDADelay  :: Int
  , timingSCLDelay  :: Int
  , timingSCLLow    :: Int
  , timingSCLHigh   :: Int
  } deriving (Eq, Show)

getTimings :: Int -> Int -> I2CTiming
getTimings pclkFreq i2cTargetFreq = case filter (\spec -> i2cTargetFreq <= rateMax spec && i2cTargetFreq >= rateMin spec) allI2CTimings of
  [spec] -> legalTimings spec pclkFreq i2cTargetFreq defaultRiseTime defaultFallTime True 0
  [] -> error $ "No valid I2C timing specification found for target frequency of " ++ show i2cTargetFreq
  _  -> error $ "Can't happen"

legalTimings :: I2CTimingSpecs
             -> Int
             -> Int
             -> Int
             -> Int
             -> Bool
             -> Int
             -> I2CTiming
legalTimings I2CTimingSpecs{..} pclkFreq i2cTargetFreq riseTime fallTime
             analogFilterEnable digitalFilterCycles =
             case legalTims of
                [] -> error "No valid I2C timings found"
                _x -> let (clkError, p, scl, sda, low, high) = minimum legalTims
                      in I2CTiming {
                           timingError     = clkError
                         , timingPrescaler = p
                         , timingSDADelay  = sda
                         , timingSCLDelay  = scl
                         , timingSCLLow    = low
                         , timingSCLHigh   = high
                         }
  where
    tBus = nsPerSec `div` i2cTargetFreq -- Peripheral bus clock period (ns)
    tI2C = nsPerSec `div` pclkFreq      -- Source clock period (ns)

    dnfDelay = digitalFilterCycles * tI2C
    -- tFilters = dnfDelay + ifAnalogFilterEnabled analogFilterDelayMin
    tSync = (ifAnalogFilterEnabled analogFilterDelayMin)
            + dnfDelay
            + 2 * tI2C
    tMax = nsPerSec `div` rateMin
    tMin = nsPerSec `div` rateMax

    ifAnalogFilterEnabled x = case analogFilterEnable of
      True -> x
      False -> 0

    legalPrescalers = nubBy (\(p1, _, _) (p2, _, _) -> p1 == p2) [
      (p, scl, sda)
      | p <- [0..15]
      , scl <- [0..15]
      , let sclDel = (scl + 1) * (p + 1) * tI2C;
      , sclDel >= sclDelayMin
      , sda <- [0..15]
      , let sdaDel = (sda * (p + 1) + 1) * tI2C;
      , sdaDel >= sdaDelayMin && sdaDel <= sdaDelayMax
      ]

    legalTims = [
      (clkError, p, scl, sda, low, high)
      | (p, scl, sda) <- legalPrescalers
      , low <- [0..255]
      , let tPresc = (p + 1) * tI2C
      , let tSCLLow = (low + 1) * tPresc + tSync;
      , tSCLLow >= lMin
      , tI2C < (tSCLLow
                - (ifAnalogFilterEnabled analogFilterDelayMin)
                - dnfDelay) `div` 4

      , high <- [0..255]
      , let tSCLHigh = (high + 1) * tPresc + tSync
      , tSCLHigh >= hMin
      , let tSCL = tSCLLow + tSCLHigh + riseTime + fallTime
      , tSCL >= tMin && tSCL <= tMax
      , tI2C < tSCLHigh
      , let clkError = abs $ tSCL - tBus
      --, clkError <= 1000 -- max 1us of error
      ]

    sdaDelayMin = max sdaDelayMin' 0
    sdaDelayMax = max sdaDelayMax' 0

    sdaDelayMin' = hddatMin + fallTime
      - (ifAnalogFilterEnabled analogFilterDelayMin)
      - (digitalFilterCycles + 3) * tI2C

    sdaDelayMax' = vddatMax - riseTime
      - (ifAnalogFilterEnabled analogFilterDelayMax)
      - (digitalFilterCycles + 4) * tI2C

    sclDelayMin = riseTime + sudatMin
