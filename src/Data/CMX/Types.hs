{-# LANGUAGE DeriveGeneric #-}
module Data.CMX.Types where

import Data.Set (Set)
import Data.Map (Map)

import GHC.Generics
import Data.Serialize
import Data.STM32.Types (Core, Family)

type Kb = Int
type Mhz = Int

data MCU = MCU {
    mcuCore        :: Core
  , mcuFamily      :: Family
  , mcuHasPowerPad :: Bool
  , mcuIoType      :: String
  , mcuDie         :: String
  , mcuLine        :: String
  , mcuPackage     :: String
  , mcuRefName     :: String
  , mcuFrequency   :: Maybe Mhz
  , mcuNumberOfIO  :: Int
  , mcuDbVersion   :: String
  , mcuRam         :: Kb
  , mcuFlash       :: Kb
  , mcuCcmRam      :: Maybe Kb
  , mcuEEProm      :: Maybe Kb
  , mcuLimits      :: [Limit]
  , mcuIps         :: Set IP
  , mcuPins        :: Set Pin
  } deriving (Generic, Eq, Ord, Show)

instance Serialize MCU

data LimitType = Min | Max | Lowest | Run
  deriving (Generic, Eq, Ord, Show)

instance Serialize LimitType

data LimitUnit = Voltage | Current | Temperature
  deriving (Generic, Eq, Ord, Show)

instance Serialize LimitUnit

data Limit = Limit {
    limitType :: LimitType
  , limitUnit :: LimitUnit
  , limitVal  :: Float
  }
  deriving (Generic, Eq, Ord, Show)

instance Serialize Limit

data IP = IP {
    ipName :: String
  , ipVersion :: String
  , ipConfigFile :: String
  , ipClockEnableMode :: String
  , ipInstanceName :: String
  } deriving (Generic, Eq, Ord, Show)

instance Serialize IP

data Pin = Pin {
    pinName :: String
  , pinType :: String
  , pinPosition :: String
  , pinSignals :: [Signal]
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Pin

data Signal = Signal {
    sigName :: String
  , sigIOModes :: String
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Signal

type Families = Map Family [SubFamily]

data SubFamily = SubFamily {
    subFamName :: String
  , subFamMCUs :: [ShortMCU]
  } deriving (Generic, Eq, Ord, Show)

data ShortMCU = ShortMCU {
    smcuName    :: String
  , smcuRefName :: String
  , smcuRPN     :: String
  , smcuRam     :: Kb
  , smcuFlash   :: Kb
  , smcuPeriphs :: [ShortPeriph]
  } deriving (Generic, Eq, Ord, Show)

data ShortPeriph = ShortPeriph {
    speriphType      :: String
  , speriphMaxOccurs :: Integer
  } deriving (Generic, Eq, Ord, Show)
