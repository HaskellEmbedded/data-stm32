{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module MakeDMAUART where

import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Char
import qualified Data.List
import qualified Data.List.NonEmpty
import qualified Data.Maybe
import Text.Mustache (ToMustache(..), object, (~>))

import MakePeriph (InstancesCtx)
import qualified MakePeriph
import qualified Contexts

data DMAUARTInstancesCtx = DMAUARTInstancesCtx
  { ddev :: String
  , dperiph :: Text
  , dinstances :: NonEmpty DMAUARTCtx
  } deriving (Show, Data, Typeable)

instance ToMustache DMAUARTInstancesCtx where
  toMustache x = object
    [ "dev" ~> ddev x
    , "periph" ~> dperiph x
    , "instances" ~> Data.List.NonEmpty.toList (dinstances x)
    , "prefixedInstances" ~> Contexts.buildPrefixed (Data.List.NonEmpty.toList $ dinstances x)
    ]

data DMAUARTCtx = DMAUARTCtx
  { uartName :: String
  , uartIndex :: Int
  , dmaIndex :: Int
  , dmaTxStream :: Int
  , dmaTxChannel :: Int
  , dmaRxStream :: Int
  , dmaRxChannel :: Int
  } deriving (Show, Data, Typeable)

instance ToMustache DMAUARTCtx where
  toMustache x = object
    [ "name" ~> uartName x
    , "dmaIndex" ~> dmaIndex x
    , "dmaTxStream" ~> dmaTxStream x
    , "dmaTxChannel" ~> dmaTxChannel x
    , "dmaRxStream" ~> dmaRxStream x
    , "dmaRxChannel" ~> dmaRxChannel x
    ]

-- Create DMAUARTCtx, mapping numeric uart index to
-- dma instance, stream and channel
--
-- The mapping is pretty arbitrary based on original implementation
dmaMapping
  :: Int -- ^ U(S)ART Index
  -> String  -- ^ U(S)ART name
  -> Maybe DMAUARTCtx
dmaMapping idx@1 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 2
    , dmaTxStream = 7
    , dmaTxChannel = 4
    , dmaRxStream = 2
    , dmaRxChannel = 4
    }
dmaMapping idx@2 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 1
    , dmaTxStream = 6
    , dmaTxChannel = 4
    , dmaRxStream = 5
    , dmaRxChannel = 4
    }
dmaMapping idx@3 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 1
    , dmaTxStream = 3
    , dmaTxChannel = 4
    , dmaRxStream = 1
    , dmaRxChannel = 4
    }
dmaMapping idx@4 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 1
    , dmaTxStream = 4
    , dmaTxChannel = 4
    , dmaRxStream = 2
    , dmaRxChannel = 4
    }
dmaMapping idx@5 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 1
    , dmaTxStream = 7
    , dmaTxChannel = 4
    , dmaRxStream = 0
    , dmaRxChannel = 4
    }
dmaMapping idx@6 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 2
    , dmaTxStream = 6
    , dmaTxChannel = 5
    , dmaRxStream = 1
    , dmaRxChannel = 5
    }
dmaMapping idx@7 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 1
    , dmaTxStream = 1
    , dmaTxChannel = 5
    , dmaRxStream = 3
    , dmaRxChannel = 5
    }
dmaMapping idx@8 name =
  Just $ DMAUARTCtx
    { uartName = name
    , uartIndex = idx
    , dmaIndex = 1
    , dmaTxStream = 0
    , dmaTxChannel = 5
    , dmaRxStream = 6
    , dmaRxChannel = 5
    }
dmaMapping _ _ = Nothing

fromInstancesCtx
  :: Text
  -> InstancesCtx
  -> DMAUARTInstancesCtx
fromInstancesCtx p x =
  DMAUARTInstancesCtx
  { ddev = MakePeriph.dev x
  , dperiph = p
  , dinstances =
      Data.List.NonEmpty.fromList
      $ Data.Maybe.catMaybes
      $ map
          (\ictx -> dmaMapping (instanceNumber $ MakePeriph.name ictx) (MakePeriph.name ictx))
          (Data.List.NonEmpty.toList $ MakePeriph.instances x)
  }
  where
    instanceNumber =
      read
      .  Data.List.dropWhile Data.Char.isLetter
