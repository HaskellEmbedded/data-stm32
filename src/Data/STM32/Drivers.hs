{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Data.STM32.Drivers where

import Data.Maybe
import qualified Data.Set as S
import qualified Data.List as L
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
    DriverInfo CAN Nothing ["bxcan1_v1_1", "bxcan1_v1_1_F7"] CommonRegTypes CommonDriver

  , DriverInfo GPIO (Just 1) ["STM32F103x8_gpio_v1_0"] VersionedRegTypes NoDriver
  , DriverInfo GPIO (Just 2) ["*"] VersionedRegTypes NoDriver

  , DriverInfo I2C (Just 1) ["i2c1_v1_5"] CommonRegTypes VersionedDriver
  , DriverInfo I2C (Just 2) ["i2c2_v1_1", "i2c2_v1_1F7"] CommonRegTypes VersionedDriver

  , DriverInfo RNG Nothing  ["rng1_v1_1", "rng1_v2_0"] NoRegTypes CommonDriver

  , DriverInfo RTC Nothing ["rtc2_v2_4"] NoRegTypes NoDriver

  , DriverInfo SPI (Just 1) ["spi2s1_v1_0", "spi2s1_v1_2"] CommonRegTypes CommonDriver -- F10x
  , DriverInfo SPI (Just 2) ["spi2s1_v2_2", "spi2s1_v2_3", "spi2s1_v2_4"] CommonRegTypes CommonDriver -- F4 and co
  , DriverInfo SPI (Just 3) ["spi2s1_v3_0", "spi2s1_v3_1", "spi2s1_v3_2", "spi2s1_v3_3", "spi2s1_v3_5"] CommonRegTypes CommonDriver -- F7 / L4

  -- v1/v2 split is artificial
  -- only because the F1 pins are setup as TX (mode output) / RX (mode input)
  -- instead of both TX & RX in mode AF as for the rest of the devices
  , DriverInfo USART (Just 1) ["sci2_v1_1"] CommonRegTypes VersionedDriver
  , DriverInfo USART (Just 2) ["sci2_v1_2"] CommonRegTypes VersionedDriver
  , DriverInfo USART (Just 3) ["sci2_v3_1", "sci3_v1_1", "sci3_v2_1"] CommonRegTypes VersionedDriver
  , DriverInfo LPUART (Just 3) ["sci2_v3_1", "sci3_v1_1", "sci3_v2_1"] CommonRegTypes VersionedDriver

  , DriverInfo IWDG Nothing ["iwdg1_v1_1", "iwdg1_v2_0"] NoRegTypes CommonDriver

  , DriverInfo EXTI (Just 1) ["*"] CommonRegTypes VersionedDriver
  , DriverInfo EXTI (Just 2) ["exti_g0"] CommonRegTypes VersionedDriver
  ]

periphDrivers p = filter (\DriverInfo{..} -> diPeriph == p) driverMapping

versioned' p = 1 < (length $ filter (\DriverInfo{..} -> diPeriph == p) driverMapping)

driverVersions = map diVersion . periphDrivers

supported :: [Periph]
supported = [UART] ++ (L.nub . map diPeriph $ driverMapping)

mcuPeriphDriver mcu periph = case periph of
  GPIO -> case mcuFamily mcu of
    F1 -> Just $ DriverInfo GPIO (Just 1) ["STM32F103x8_gpio_v1_0"] VersionedRegTypes NoDriver
    _  -> Just $ DriverInfo GPIO (Just 2) ["*"] VersionedRegTypes NoDriver
  EXTI -> case mcuFamily mcu of
    G0 -> Just $ DriverInfo EXTI (Just 2) ["exti_g0"] CommonRegTypes VersionedDriver
    _  -> Just $ DriverInfo EXTI (Just 1) ["*"] CommonRegTypes VersionedDriver
  _ -> case filter (mcuCompatible mcu) (periphDrivers $ asUSART periph) of
          [x] -> Just x
          _   -> Nothing
  where asUSART UART = USART
        asUSART x = x

-- error $ "Multiple or no drivers found for periph and mcu: " ++ show periph ++ ", " ++ mcuRefName mcu

mcuCompatible MCU{..} DriverInfo{..} = case diCompatibleIps of
  ["*"] -> True
  _     -> not
             $ S.null
             $ S.intersection (S.fromList diCompatibleIps)
                              (S.map ipVersion mcuIps)

hasPeriph mcu periph =  not . S.null . S.filter (\ip -> ipName ip == show periph) $ mcuIps mcu

hasDriver mcu periph = isJust $ mcuPeriphDriver mcu periph

someMCUWithDriver :: DriverInfo -> [MCU] -> MCU
someMCUWithDriver di ms = case filter (flip mcuCompatible di) ms of
  [] -> error $ "No MCU for driver " ++ show di
  (x:_) -> x
