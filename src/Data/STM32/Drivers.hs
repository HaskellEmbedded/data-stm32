{-# LANGUAGE RecordWildCards #-}
module Data.STM32.Drivers where

import qualified Data.Maybe
import qualified Data.Set
import qualified Data.List
import Data.STM32.Types
import Data.CMX

-- | Are register types (Peripheral/RegTypes.hs)
-- for this peripheral common for each driver or versioned?
data RegTypes =
    NoRegTypes        -- ^ Has no register types
  | CommonRegTypes    -- ^ Common register types for all versions
  | VersionedRegTypes -- ^ Versioned register types
  deriving (Show, Eq)

-- | Is driver for this peripheral common or versioned
data Driver =
    NoDriver         -- ^ No driver (e.g. GPIO)
  | CommonDriver     -- ^ Common driver for all versions
  | VersionedDriver  -- ^ Versioned drivers
  deriving (Show, Eq)

-- | Match devices
data Match =
    ByFam Family -- ^ By @Family@
  | ByDev String -- ^ By device string
  | ByIP String  -- ^ By IP block version
  | Not Match    -- ^ Negated Match
  deriving (Show, Eq)

-- | Description of a specific driver for @Periph@
data DriverInfo = DriverInfo {
    diPeriph         :: Periph     -- ^ Peripheral
  , diVersion        :: Maybe Int  -- ^ @Nothing@ if there's only one driver version, @Just version@ for versioned
  , diRepresentative :: Match      -- ^ Representative device using this driver, used for generating registers
  , diCompatible     :: [Match]    -- ^ Compatible devices, if empty only @diRepresentative@ matches
  , diRegTypes       :: RegTypes   -- ^ Versioned, common or no register types (RegTypes.hs)
  , diDriver         :: Driver     -- ^ Versioned, common or no driver
  } deriving (Show)

driverMapping :: [DriverInfo]
driverMapping = [
    DriverInfo CAN Nothing (ByIP "bxcan1_v1_1")
      [ByIP "bxcan1_v1_1_F7", ByIP "bxcan1_v1_1_F1"] CommonRegTypes CommonDriver

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

  , DriverInfo SYSCFG Nothing (ByDev "F765")
      [ Not (ByFam F1) ] NoRegTypes NoDriver

  , DriverInfo AFIO Nothing (ByFam F1)
      [ ByFam F1 ] NoRegTypes NoDriver
  ]

-- | List of drivers for specific @Periph@
periphDrivers :: Periph -> [DriverInfo]
periphDrivers p = filter (\DriverInfo{..} -> diPeriph == p) driverMapping

-- | Is this @Periph@ versioned? Meaning has it more than one version across all devices?
versioned' :: Periph -> Bool
versioned' p = 1 < length (filter (\DriverInfo{..} -> diPeriph == p) driverMapping)

-- | List of driver versions for @Periph@
driverVersions :: Periph -> [Maybe Int]
driverVersions = map diVersion . periphDrivers

-- | Supported peripherals (peripherals with known driver).
-- @UART@ is listed here since it is treated similar to @USART@ peripheral
supported :: [Periph]
supported = UART:(Data.List.nub . map diPeriph $ driverMapping)

-- | Try to find @DriverInfo@ for concrete @MCU@ and @Periph@
mcuPeriphDriver :: MCU -> Periph -> Maybe DriverInfo
mcuPeriphDriver mcu periph = case filter (mcuCompatible mcu) (periphDrivers $ asUSART periph) of
  [x] -> Just x
  _   -> Nothing
  where asUSART UART = USART
        asUSART x = x

-- | Is this @MCU@ compatible with @DriverInfo@?
mcuCompatible :: MCU -> DriverInfo -> Bool
mcuCompatible mcu DriverInfo{..} = any (isCompat mcu) (diRepresentative:diCompatible)

-- XXX: Turn dev into like (st # family F7 # dev "65")
-- and turn that into ByIP and call isCompat again.
-- Then we can use (ByName "F405") in `diCompatible` as well.
isCompat :: MCU -> Match -> Bool
isCompat MCU{..} (ByDev name) = matches ("STM32" ++ name) mcuName
isCompat MCU{..} (ByFam f) = mcuFamily == f
isCompat x (Not m) = not $ isCompat x m
isCompat MCU{..} (ByIP name) = Data.Set.member name (Data.Set.map ipVersion mcuIps)

-- | True if this @MCU@ has @Periph@
hasPeriph :: MCU -> Periph -> Bool
hasPeriph mcu periph =
    not
  . Data.Set.null
  . Data.Set.filter (\ip -> ipName ip == show periph)
  $ mcuIps mcu

-- | True if there's a known driver for @MCU@s @Periph@
hasDriver :: MCU -> Periph -> Bool
hasDriver mcu periph = Data.Maybe.isJust $ mcuPeriphDriver mcu periph

-- | Find some @MCU@ for specified @DriverInfo@
someMCUWithDriver :: DriverInfo -> [MCU] -> MCU
someMCUWithDriver di ms =
  case filter (\mcu -> isCompat mcu (diRepresentative di)) ms of
    [] -> error $ "No MCU for driver " ++ show di
    (x:_) -> x
