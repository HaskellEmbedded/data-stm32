{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Contexts where

import Data.Text (Text)
import Data.Data (Data, Typeable)

import Text.Mustache

data ClocksCtx = ClocksCtx { clocks :: [ClockCtx] }
  deriving (Show, Data, Typeable)

instance ToMustache ClocksCtx where
  toMustache x = object
    [ "clocks" ~> clocks x
    , "clocksPrefixed" ~> buildPrefixed (clocks x)
    ]

data ClockCtx = ClockCtx { clockName :: String, clockHz :: String }
  deriving (Show, Data, Typeable)

instance ToMustache ClockCtx where
  toMustache x = object
    [ "clockName" ~> clockName x
    , "clockHz" ~> clockHz x
    ]

data Prefixed a = Prefixed {
    prefixedPrefix :: String
  , prefixedData :: a
  }
  deriving (Show, Data, Typeable)

buildPrefixed :: [a] -> [Prefixed a]
buildPrefixed = buildPrefixedWith ","

buildPrefixedWith :: String -> [a] -> [Prefixed a]
buildPrefixedWith pfx xs =
  map
    (\(x, noPrefix) ->
      Prefixed
        { prefixedPrefix = if noPrefix then " " else pfx
        , prefixedData = x
        })
    $ zip xs (True: [False, False ..])

instance ToMustache a => ToMustache (Prefixed a) where
  toMustache x = object
    [ "prefix" ~> prefixedPrefix x
    , "data"   ~> prefixedData x
    ]

data ImportsCtx = ImportsCtx
  { importsDev :: String
  , importsImports :: [String]
  }
  deriving (Show, Data, Typeable)

instance ToMustache ImportsCtx where
  toMustache x = object
    [ "imDev" ~> importsDev x
    , "imImports" ~> buildPrefixed (importsImports x)
    ]

data VersionsCtx = VersionsCtx { versions :: [VersionCtx] }
  deriving (Show, Data, Typeable)

instance ToMustache VersionsCtx where
  toMustache x = object [ "versions" ~> versions x ]

data VersionCtx = VersionCtx { prefix :: String, version :: String }
  deriving (Show, Data, Typeable)

instance ToMustache VersionCtx where
  toMustache x = object
    [ "prefix" ~> prefix x
    , "version" ~> version x ]

data FamiliesCtx = FamiliesCtx { families :: [String] }
  deriving (Show, Data, Typeable)

instance ToMustache FamiliesCtx where
  toMustache x = object [ "families" ~> families x ]

data ShortDevicesCtx = ShortDevicesCtx { shortDevices :: [String] }
  deriving (Show, Data, Typeable)

instance ToMustache ShortDevicesCtx where
  toMustache x = object [ "shortDevices" ~> shortDevices x ]

data RegsCtx = RegsCtx {
    imports :: [Text]
  , regs    :: Text
  } deriving (Show, Data, Typeable)

instance ToMustache RegsCtx where
  toMustache x = object
    [ "imports" ~> imports x
    , "regs" ~> regs x
    ]

data PeriphCtx = PeriphCtx {
    typ           :: String
  , pversion       :: String
  , bitDataRegs   :: String
  , bitDataRegsMk :: String
  } deriving (Show, Data, Typeable)

instance ToMustache PeriphCtx where
  toMustache x = object
    [ "type" ~> typ x
    , "version" ~> pversion x
    , "bitDataRegs" ~> bitDataRegs x
    , "bitDataRegsMk" ~> bitDataRegsMk x
    ]
