{-# LANGUAGE DeriveDataTypeable #-}

module Contexts where

import Data.Text (Text)
import Data.Data (Data, Typeable)

data ClocksCtx = ClocksCtx { clocks :: [ClockCtx] }
  deriving (Show, Data, Typeable)

data ClockCtx = ClockCtx { clockName :: String, clockHz :: String }
  deriving (Show, Data, Typeable)

data ImportsCtx = ImportsCtx { imDev :: String, imImports :: [String] }
  deriving (Show, Data, Typeable)

data VersionsCtx = VersionsCtx { versions :: [VersionCtx] }
  deriving (Show, Data, Typeable)

data VersionCtx = VersionCtx { prefix :: String, version :: String }
  deriving (Show, Data, Typeable)

data FamiliesCtx = FamiliesCtx { families :: [String] }
  deriving (Show, Data, Typeable)

data ShortDevicesCtx = ShortDevicesCtx { shortDevices :: [String] }
  deriving (Show, Data, Typeable)

data RegsCtx = RegsCtx {
    imports :: [Text]
  , regs    :: Text
  } deriving (Show, Data, Typeable)

data PeriphCtx = PeriphCtx {
    typ           :: String
  , pversion       :: String
  , bitDataRegs   :: String
  , bitDataRegsMk :: String
  } deriving (Show, Data, Typeable)
