{-# LANGUAGE DeriveDataTypeable #-}

module Contexts where

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


