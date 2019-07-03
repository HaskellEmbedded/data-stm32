{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Data.STM32.Drivers where

import qualified Data.Set as S
import Data.STM32.Types
import Data.CMX

data RegTypes = NoRegTypes | CommonRegTypes | VersionedRegTypes
  deriving (Show, Eq)

data Driver = NoDriver | CommonDriver | VersionedDriver
  deriving (Show, Eq)

data DriverInfo = DriverInfo {
    diPeriph :: Periph
  , diVersion :: Maybe Int
  , diCompatibleIps :: [String]
  , diRegTypes :: RegTypes
  , diDriver :: Driver
  } deriving (Show)

driverMapping = [
    DriverInfo RNG Nothing  ["rng1_v1_1"] NoRegTypes CommonDriver
  , DriverInfo I2C (Just 1) ["i2c1_v1_5"] NoRegTypes VersionedDriver
  , DriverInfo I2C (Just 2) ["i2c2_v1_1", "i2c2_v1_1F7"] NoRegTypes VersionedDriver
  ]

periphDrivers p = filter (\DriverInfo{..} -> diPeriph == p) driverMapping

versioned' p = 1 < (length $ filter (\DriverInfo{..} -> diPeriph == p) driverMapping)

driverVersions = map diVersion . periphDrivers

mcuCompatible MCU{..} DriverInfo{..} = not
                                     $ S.null
                                     $ S.intersection (S.fromList diCompatibleIps) (S.map ipVersion mcuIps)

someMCUWithDriver :: DriverInfo -> [MCU] -> MCU
someMCUWithDriver di ms = case filter (flip mcuCompatible di) ms of
  [] -> error $ "No MCU for driver " ++ show di
  (x:_) -> x
