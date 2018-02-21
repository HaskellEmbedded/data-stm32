module Data.CMX.Types where

import qualified Data.Set as Set

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
  } deriving (Eq, Ord, Show)

data LimitType = Min | Max | Lowest | Run
  deriving (Eq, Ord, Show)

data LimitUnit = Voltage | Current | Temperature
  deriving (Eq, Ord, Show)

data Limit = Limit {
    limitType :: LimitType
  , limitUnit :: LimitUnit
  , limitVal  :: Float
  }
  deriving (Eq, Ord, Show)

data IP = IP {
    ipName :: String
  , ipVersion :: String
  , ipConfigFile :: String
  , ipClockEnableMode :: String
  , ipInstanceName :: String
  } deriving (Eq, Ord, Show)

data Pin = Pin {
    pinName :: String
  , pinType :: String
  , pinPosition :: String
  , pinSignals :: [Signal]
  } deriving (Eq, Ord, Show)

data Signal = Signal {
    sigName :: String
  , sigIOModes :: String
  } deriving (Eq, Ord, Show)

