{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Parse.MCU (parseMCU) where

import Control.Monad

import Control.Arrow.ArrowList
import Text.XML.HXT.Core
import qualified Data.Char as Char
import qualified Data.Set as Set
import Data.Maybe

import Data.CMX.Types

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = text <<< atTag tag
att  = getAttrValue
-- nonempty attr value
attNE x = (getAttrValue x >>> isA (/= ""))
attMaybe attname tagname = withDefault (arr Just <<< attNE attname <<< atTag tagname) Nothing

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

-- db/package.xml parser
getDBVerRel = atTag "Package" >>>
  proc x -> do
    version <- getAttrValue "DBVersion" -< x
    release <- getAttrValue "Release" <<< deep (atTag "PackDescription") -< x
    returnA -< (version, release)

-- devices/.db parser
getDB = atTag "family" >>>
  proc x -> do
    name <- getAttrValue "name" -< x
    core <- text <<< atTag "CPUcore" -< x
    header <- text <<< atTag "header" -< x

    families <- listA subFamily -< x
    returnA -< (name, core, families)

subFamily = atTag "subFamily" >>>
  proc x -> do
    name <- getAttrValue "name" -< x
    fpu <- getAttrValue "fpu" -< x
    clock <- getAttrValue "clock" -< x
    devices <- listA device -< x
    returnA -< (name, fpu, clock, devices)

device = atTag "device" >>>
  proc x -> do
    partNumbers <- text <<< atTag "PN" -< x
    variants <- text <<< atTag "variants" -< x
    memories <- listA (memory "") -< x
    memoriesITCM <- listA (memory "ITCM") -< x
    header <- text <<< atTag "header" -< x
    returnA -< (partNumbers, variants, memories, memoriesITCM, header)

memory t = atTag ("memory" ++ t) >>>
  proc x -> do
    name <- att "name" -< x
    access <- att "access" -< x
    start <- att "start" -< x
    size <- att "size" -< x
    returnA -< (name, access, start, size)

-- mcu/STM32F*.xml parser
mcu = atTag "Mcu" >>>
  proc x -> do
    mcuClockTree <- att "ClockTree" -< x
    mcuDbVersion <- att "DBVersion" -< x
    mcuFamily <- att "Family" -< x
    mcuIoType <- att "IOType" -< x
    mcuLine <- att "Line" -< x
    mcuPackage <- att "Package" -< x
    mcuRefName <- att "RefName" -< x
    mcuFrequency <- withDefault (arr (Just . read) <<< textAtTag "Frequency") Nothing -< x
    mcuDie <- textAtTag "Die" -< x
    mcuCcmRam <- withDefault (arr (Just . read) <<< textAtTag "CCMRam") Nothing -< x
    mcuEEProm <- withDefault (arr (Just . (`div` 1024) . read) <<< textAtTag "E2prom") Nothing -< x
    mcuCore <- textAtTag "Core" -< x

    hasPowerPad' <- att "HasPowerPad" -< x
    ramVariants' <- listA (textAtTag "Ram") -< x
    flashVariants' <- listA (textAtTag "Flash") -< x
    numberOfIO' <- textAtTag "IONb" -< x

    voltageMin <- attMaybe "Min" "Voltage" -< x
    voltageMax <- attMaybe "Max" "Voltage" -< x

    temperatureMin <- attMaybe "Min" "Temperature" -< x
    temperatureMax <- attMaybe "Max" "Temperature" -< x

    currentLowest <- attMaybe "Lowest" "Current"-< x
    currentRun <- attMaybe "Run" "Current" -< x

    ips' <- listA ip -< x
    pins' <- listA pin -< x

    let
      mcuIps = Set.fromList ips'
      mcuPins = Set.fromList pins'
      --mcuRamVariants = map read ramVariants'
      --mcuFlashVariants = map read flashVariants'
      -- we fill these instead with values from mcu/families.xml
      mcuRam = 0
      mcuFlash = 0
      mcuHasPowerPad = read $ capitalized hasPowerPad'
      mcuNumberOfIO = read numberOfIO'
      mcuLimits = catMaybes [
          maybe Nothing (Just . Limit Min Voltage . read) voltageMin
        , maybe Nothing (Just . Limit Max Voltage . read) voltageMax

        , maybe Nothing (Just . Limit Min Temperature . read) temperatureMin
        , maybe Nothing (Just . Limit Max Temperature . read) temperatureMax

        , maybe Nothing (Just . Limit Lowest Current . read) currentLowest
        , maybe Nothing (Just . Limit Run Current . read) currentRun
        ]

    returnA -< MCU{..}

ip = atTag "IP" >>>
  proc x -> do
    ipName <- att "Name" -< x
    ipVersion <- att "Version" -< x
    ipConfigFile <- att "ConfigFile" -< x
    ipClockEnableMode <- att "clockEnableMode" -< x
    ipInstanceName <- att "InstanceName" -< x
    returnA -< IP{..}


pin = atTag "Pin" >>>
  proc x -> do
    pinName <- att "Name" -< x
    pinPosition <- att "Position" -< x
    pinType <- att "Type" -< x
    pinSignals <- listA signal -< x

    returnA -< Pin{..}

signal = atTag "Signal" >>>
  proc x -> do
    sigName <- att "Name" -< x
    sigIOModes <- att "IOModes" -< x
    returnA -< Signal{..}

parseMCU f = do
  res <- runX (readDocument [] f >>> mcu)
  case res of
    []  -> return $ error $ "no mcu parsed from " ++ f
    [x] -> return x
    _   -> return $ error $ "multiple mcus parsed from " ++ f
