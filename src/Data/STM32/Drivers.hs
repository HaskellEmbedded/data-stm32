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

data Match = ByFam Family | ByDev String | ByIP String | Not Match
  deriving (Show, Eq)

data DriverInfo = DriverInfo {
    diPeriph :: Periph
  , diVersion :: Maybe Int
  , diRepresentative :: Match
  , diCompatible :: [Match]
  , diRegTypes :: RegTypes
  , diDriver :: Driver
  } deriving (Show)

driverMapping = [
    DriverInfo CAN Nothing (ByIP "bxcan1_v1_1")
      [ByIP "bxcan1_v1_1_F7"] CommonRegTypes CommonDriver

  , DriverInfo GPIO (Just 1) (ByFam F1)
      [ByIP "STM32F103x8_gpio_v1_0"] VersionedRegTypes NoDriver
  , DriverInfo GPIO (Just 2) (ByDev "F765")
      [Not (ByFam F1)] VersionedRegTypes NoDriver

  , DriverInfo I2C (Just 1) (ByIP "i2c1_v1_5")
      [] CommonRegTypes VersionedDriver
  , DriverInfo I2C (Just 2) (ByIP "i2c2_v1_1")
      [ByIP "i2c2_v1_1F7"] CommonRegTypes VersionedDriver

  , DriverInfo RNG Nothing  (ByIP "rng1_v2_0")
      [ByIP "rng1_v1_1"] NoRegTypes CommonDriver

  , DriverInfo RTC Nothing (ByIP "rtc2_v2_4")
      [] NoRegTypes NoDriver

  -- F10x
  , DriverInfo SPI (Just 1) (ByIP "spi2s1_v1_0")
      [ByIP "spi2s1_v1_2"] CommonRegTypes CommonDriver
  -- F4 and co
  , DriverInfo SPI (Just 2) (ByIP "spi2s1_v2_2")
      [ByIP "spi2s1_v2_3", ByIP "spi2s1_v2_4"] CommonRegTypes CommonDriver
  -- F7 / L4
  , DriverInfo SPI (Just 3) (ByIP "spi2s1_v3_0")
      (map ByIP
        [ "spi2s1_v3_1"
        , "spi2s1_v3_2"
        , "spi2s1_v3_3"
        , "spi2s1_v3_5"]) CommonRegTypes CommonDriver

  -- v1/v2 split is artificial
  -- only because the F1 pins are setup as TX (mode output) / RX (mode input)
  -- instead of both TX & RX in mode AF as for the rest of the devices
  , DriverInfo USART (Just 1) (ByIP "sci2_v1_1")
      [] CommonRegTypes VersionedDriver
  , DriverInfo USART (Just 2) (ByIP "sci2_v1_2")
      [] CommonRegTypes VersionedDriver
  , DriverInfo USART (Just 3) (ByIP "sci3_v1_1")
      (map ByIP
        [ "sci2_v2_0"
        , "sci2_v2_1"
        , "sci2_v2_2"
        , "sci2_v3_1"
        , "sci3_v1_0"
        , "sci3_v2_1"]) CommonRegTypes VersionedDriver

  , DriverInfo LPUART (Just 3) (ByIP "sc3_v2_1")
      (map ByIP ["sci2_v3_1", "sci3_v1_1", "sci3_v2_1"]) CommonRegTypes VersionedDriver

  , DriverInfo IWDG Nothing (ByIP "iwdg1_v2_0")
      (map ByIP ["iwdg1_v1_1", "iwdg1_v2_0"]) NoRegTypes CommonDriver

  , DriverInfo EXTI (Just 1) (ByDev "F765")
      [Not (ByFam G0)] CommonRegTypes VersionedDriver
  , DriverInfo EXTI (Just 2) (ByFam G0)
      [ByIP "exti_g0"] CommonRegTypes VersionedDriver
  ]

periphDrivers p = filter (\DriverInfo{..} -> diPeriph == p) driverMapping

versioned' p = 1 < (length $ filter (\DriverInfo{..} -> diPeriph == p) driverMapping)

driverVersions = map diVersion . periphDrivers

supported :: [Periph]
supported = [UART] ++ (L.nub . map diPeriph $ driverMapping)

mcuPeriphDriver mcu periph = case periph of
  _ -> case filter (mcuCompatible mcu) (periphDrivers $ asUSART periph) of
          [x] -> Just x
          _   -> Nothing
  where asUSART UART = USART
        asUSART x = x

mcuCompatible mcu DriverInfo{..} = any (isCompat mcu) (diRepresentative:diCompatible)

-- XXX: Turn dev into like (st # family F7 # dev "65")
-- and turn that into ByIP and call isCompat again.
-- Then we can use (ByName "F405") in `diCompatible` as well.
isCompat MCU{..} (ByDev name) = matches ("STM32" ++ name) mcuName
isCompat MCU{..} (ByFam f) = mcuFamily == f
isCompat x (Not m) = not $ isCompat x m
isCompat MCU{..} (ByIP name) = S.member name (S.map ipVersion mcuIps)

hasPeriph mcu periph =  not . S.null . S.filter (\ip -> ipName ip == show periph) $ mcuIps mcu

hasDriver mcu periph = isJust $ mcuPeriphDriver mcu periph

someMCUWithDriver :: DriverInfo -> [MCU] -> MCU
someMCUWithDriver di ms =
  case filter (\mcu -> isCompat mcu (diRepresentative di)) ms of
    [] -> error $ "No MCU for driver " ++ show di
    (x:_) -> x
