{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SVD.Parse where

import Safe
import Control.Monad

import Control.Arrow.ArrowList
import Text.XML.HXT.Core
import qualified Data.Char as Char

import Data.SVD.Types

-- atTag doesn't uses deep here
atTag tag = getChildren >>> hasName tag
text = getChildren >>> getText
textAtTag tag = text <<< atTag tag
att  = getAttrValue
-- nonempty attr value
attNE x = (getAttrValue x >>> isA (/= ""))
attMaybe attname tagname = withDefault (arr Just <<< attNE attname <<< atTag tagname) Nothing


filterCrap = unwords . words . filter (\c -> Char.ord c < 127) . filter ( not . (flip elem ['\n', '\t', '\r']))

-- svd parser
svd = atTag "device" >>>
  proc x -> do
    --name <- text <<< hasName "name" <<< getChildren -< x
    deviceName <- textAtTag "name" -< x
    deviceVersion <- textAtTag "version" -< x
    desc <- textAtTag "description" -< x
    addressUnitBits' <- textAtTag "addressUnitBits" -< x
    width' <- textAtTag "width" -< x
    size' <- textAtTag "size" -< x
    resetValue' <- textAtTag "resetValue" -< x
    resetMask' <- textAtTag "resetMask" -< x

    let deviceAddressUnitBits = read addressUnitBits'
        deviceWidth = read width'
        deviceSize = read size'
        deviceResetValue = read resetValue'
        deviceResetMask = read resetMask'
        deviceDescription = filterCrap desc

    devicePeripherals <- listA peripheral <<< atTag "peripherals" -< x

    returnA -< Device{..}

peripheral = atTag "peripheral" >>>
  proc x -> do
    periphName <- textAtTag "name" -< x
    periphDerivedFrom <- withDefault (arr Just <<< isA (/= "") <<< att "derivedFrom") Nothing -< x
    desc <- withDefault (textAtTag "description") "" -< x
    periphGroupName <- withDefault (textAtTag "groupName") "" -< x
    baseAddress' <- textAtTag "baseAddress" -< x
    periphAddressBlock <- withDefault (arr Just <<< addressBlock) Nothing -< x

    periphInterrupts <- listA interrupt -< x

    periphRegisters <- listA register -< x

    let periphBaseAddress = read baseAddress'
        periphDescription = filterCrap desc

    returnA -< Peripheral{..}

addressBlock = atTag "addressBlock" >>>
  proc x -> do
    offset <- textAtTag "offset" -< x
    size <- textAtTag "size" -< x
    addressBlockUsage <- textAtTag "usage" -< x

    let addressBlockOffset = read offset
        addressBlockSize = read size

    returnA -< AddressBlock{..}

interrupt = atTag "interrupt" >>>
  proc x -> do
    name <- textAtTag "name" -< x
    desc <- textAtTag "description" -< x
    val <- textAtTag "value" -< x

    let interruptName = map Char.toUpper name
        interruptValue = read val
        interruptDescription = filterCrap desc

    returnA -< Interrupt{..}

register = atTag "registers" >>> atTag "register" >>>
  proc x -> do
    regName <- textAtTag "name" -< x
    regDisplayName <- textAtTag "displayName" -< x
    desc <- textAtTag "description" -< x

    offset <- textAtTag "addressOffset" -< x
    size <- textAtTag "size" -< x
    access <- withDefault (textAtTag "access") "read-write" -< x
    resetValue <- textAtTag "resetValue" -< x

    regFields <- listA field <<< atTag "fields" -< x

    let regAddressOffset = read offset
        regSize = read size
        regAccess = toAccessType access
        regResetValue = read resetValue
        regDescription = filterCrap desc

    returnA -< Register{..}

field = atTag "field" >>>
  proc x -> do
    fieldName <- textAtTag "name" -< x
    desc <- textAtTag "description" -< x
    bitOffset <- textAtTag "bitOffset" -< x
    bitWidth <- textAtTag "bitWidth" -< x

    let fieldBitOffset = read bitOffset
        fieldBitWidth = read bitWidth
        fieldDescription = filterCrap desc
        fieldReserved = False
        fieldRegType = Nothing

    returnA -< Field{..}

parseSVD f = do
  res <- runX (readDocument [] f >>> svd)
  case res of
    [] -> return $ Left "no device parsed"
    [x] -> return $ Right x
    _ -> return $ Left $ "multiple devices parsed"
