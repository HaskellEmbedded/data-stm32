{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SVD.Parse where

import Safe

import Control.Arrow.ArrowList
import qualified Data.Char as Char
import qualified Data.Maybe
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core

import Data.SVD.Types

-- atTag doesn't uses deep here
atTag :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
atTag tag = getChildren >>> hasName tag

text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText

textAtTag :: ArrowXml cat => String -> cat (NTree XNode) String
textAtTag tag = text <<< atTag tag

textAtTagOrEmpty :: ArrowXml cat => String -> cat (NTree XNode) String
textAtTagOrEmpty tag = withDefault (text <<< atTag tag) ""

att :: ArrowXml cat => String -> cat XmlTree String
att  = getAttrValue

-- nonempty attr value
attNE :: ArrowXml cat => String -> cat XmlTree String
attNE x = getAttrValue x >>> isA (/= "")

attMaybe :: ArrowXml cat => String -> String -> cat (NTree XNode) (Maybe String)
attMaybe attname tagname =
  withDefault
    (arr Just <<< attNE attname <<< atTag tagname)
    Nothing

filterCrap :: String -> String
filterCrap =
  unwords
  . words
  . filter (\c -> Char.ord c < 127)
  . filter ( not . (`elem` ['\n', '\t', '\r']))

-- svd parser
svd :: ArrowXml cat => cat (NTree XNode) Device
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

-- loose version of svd that doesn't require device properties
svdPeripherals :: ArrowXml cat => cat (NTree XNode) [Peripheral]
svdPeripherals = atTag "device" >>>
  proc x -> do
    devicePeripherals <- listA peripheral <<< atTag "peripherals" -< x
    returnA -< devicePeripherals

peripheral :: ArrowXml cat => cat (NTree XNode) Peripheral
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

addressBlock
  :: ArrowXml cat
  => cat (NTree XNode) AddressBlock
addressBlock = atTag "addressBlock" >>>
  proc x -> do
    offset <- textAtTag "offset" -< x
    size <- textAtTag "size" -< x
    addressBlockUsage <- textAtTag "usage" -< x

    let addressBlockOffset = read offset
        addressBlockSize = read size

    returnA -< AddressBlock{..}

interrupt
  :: ArrowXml cat
  => cat (NTree XNode) Interrupt
interrupt = atTag "interrupt" >>>
  proc x -> do
    name <- textAtTag "name" -< x
    desc <- textAtTag "description" -< x
    val <- textAtTag "value" -< x

    let interruptName = map Char.toUpper name
        interruptValue = read val
        interruptDescription = filterCrap desc

    returnA -< Interrupt{..}

register
  :: ArrowXml cat
  => cat (NTree XNode) Register
register = atTag "registers" >>> atTag "register" >>>
  proc x -> do
    regName <- textAtTag "name" -< x
    regDisplayName <- textAtTagOrEmpty "displayName" -< x
    desc <- textAtTagOrEmpty "description" -< x

    offset <- textAtTag "addressOffset" -< x
    size <- textAtTag "size" -< x
    access <- withDefault (textAtTag "access") "read-write" -< x

    regResetValue <- withDefault (arr (Just . read) <<< textAtTag "resetValue") Nothing -< x
    regFields <- withDefault (listA field <<< atTag "fields") [] -< x

    let regAddressOffset = read offset
        regSize = read size
        regAccess = toAccessType access
        regDescription = filterCrap desc

    returnA -< Register{..}

field
  :: ArrowXml cat
  => cat (NTree XNode) Field
field = atTag "field" >>>
  proc x -> do
    fieldName <- textAtTag "name" -< x
    desc <- textAtTagOrEmpty "description" -< x

    bitOffsetMay <- withDefault (arr (Just . read) <<< textAtTag "bitOffset") Nothing -< x
    bitWidthMay <- withDefault (arr (Just . read) <<< textAtTag "bitWidth") Nothing -< x

    -- bitRange [MSB:LSB]
    bitRange <- withDefault (arr (Just . splitRange) <<< textAtTag "bitRange") Nothing -< x

    -- XXX: one more possibility is lsb msb tags format, handle if needed

    let errmsg = error "Neither bitRange nor bitOffset + bitWidth defined"
        (fieldBitOffset, fieldBitWidth) = case bitRange of
            Nothing -> ( Data.Maybe.fromMaybe errmsg bitOffsetMay
                        , Data.Maybe.fromMaybe errmsg bitWidthMay)
            Just (msb, lsb) -> (lsb, msb - lsb + 1)

        fieldDescription = filterCrap desc
        fieldReserved = False
        fieldRegType = Nothing

    returnA -< Field{..}
    where
      splitRange :: String -> (Int, Int)
      splitRange r = (readNote "splitRange" $ takeWhile (/=':') raw,
                      readNote "splitRange" $ drop 1 $ dropWhile (/=':') raw)
        where
          raw = drop 1 $ init r

parseSVD :: String -> IO (Either String Device)
parseSVD f = do
  res <- runX (readDocument [] f >>> svd)
  case res of
    [] -> return $ Left "no device parsed"
    [x] -> return $ Right x
    _ -> return $ Left "multiple devices parsed"

parseSVDPeripherals :: String -> IO (Either String [Peripheral])
parseSVDPeripherals f = do
  res <- runX (readDocument [] f >>> svdPeripherals)
  case res of
    [] -> return $ Left "no peripherals parsed"
    [x] -> return $ Right x
    _ -> return $ Left "multiple devices parsed"
