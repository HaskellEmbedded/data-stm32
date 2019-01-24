{-# LANGUAGE DeriveGeneric #-}
module Data.CMX.Types where

import qualified Data.Set as Set

import GHC.Generics
import Data.Serialize

type Kb = Int
type Mhz = Int

data MCU = MCU {
    mcuCore :: String
  , mcuFamily :: String
  , mcuHasPowerPad :: Bool
  , mcuIoType :: String
  , mcuDie :: String
  , mcuLine :: String
  , mcuPackage :: String
  , mcuRefName :: String
  , mcuFrequency :: Maybe Mhz
  , mcuNumberOfIO :: Int
  , mcuDbVersion :: String
  , mcuRamVariants :: [Kb]
  , mcuFlashVariants :: [Kb]
  , mcuCcmRam :: Maybe Kb
  , mcuLimits :: [Limit]
  , mcuIps :: Set.Set IP
  , mcuPins :: Set.Set Pin
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

