{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Parse.MCU (parseMCU, parseAFs, parseClockSources) where

import Control.Monad

import Control.Arrow.ArrowList
import Text.XML.HXT.Core
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.List (nub, nubBy, isInfixOf, isSuffixOf)

import Data.CMX.Types
import Data.STM32.Types
import Data.STM32.Clock
import Data.STM32.Name

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
mcu mcuRefName = atTag "Mcu" >>>
  proc x -> do
    mcuClockTree <- att "ClockTree" -< x
    mcuDbVersion <- att "DBVersion" -< x
    mcuFamily' <- att "Family" -< x
    mcuIoType <- att "IOType" -< x
    mcuLine <- att "Line" -< x
    mcuPackage <- att "Package" -< x
    -- passed from families parser as here it contains things like (B-C)
    --mcuRefName <- att "RefName" -< x
    mcuFrequency <- withDefault (arr (Just . (*(10^6)) . read) <<< textAtTag "Frequency") Nothing -< x
    mcuDie <- textAtTag "Die" -< x
    mcuCcmRam <- withDefault (arr (Just . (*1024) . read) <<< textAtTag "CCMRam") Nothing -< x
    mcuEEProm <- withDefault (arr (Just . read) <<< textAtTag "E2prom") Nothing -< x
    -- XXX: we ignore that mcu such as MP1 family can have multiple cores
    -- which results in multiple mcu's parsed at parseMCU, we don't currently support these anyway
    mcuCore <- (arr parseCore <<< textAtTag "Core") -< x

    hasPowerPad' <- att "HasPowerPad" -< x
    --these are added/computed from families.xml later
    --ramVariants' <- listA (textAtTag "Ram") -< x
    --flashVariants' <- listA (textAtTag "Flash") -< x
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
      mcuFamily = nameToFamily mcuFamily'
      mcuName = case parseName $ B.pack mcuRefName of
        Left err -> error $ "Unable to parse mcu name out of " ++ mcuRefName
        Right x -> x
      mcuIps = Set.fromList ips'
      mcuPins = Set.fromList pins'
      --mcuRamVariants = map read ramVariants'
      --mcuFlashVariants = map read flashVariants'
      -- we fill these instead with values from mcu/families.xml
      mcuRam = 0
      mcuFlash = 0
      -- no information about these in xml files,
      -- added afterwards in Extract
      mcuRam1 = 0
      mcuRam2 = Nothing
      mcuRam3 = Nothing

      mcuItcmRam = Nothing
      mcuBackupRam = Nothing

      mcuForceSplit = False

      mcuClocks = []

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

parseCore = match . drop 4
  where
    match "Cortex-M0"  = CortexM0
    match "Cortex-M0+" = CortexM0Plus
    match "Cortex-M3"  = CortexM3
    match "Cortex-M33" = CortexM33
    match "Cortex-M4"  = CortexM4F
    match "Cortex-M7"  = CortexM7F
    match "Cortex-A7"  = CortexA7
    match unknownCore = error $ "Unknow core" ++ unknownCore

parseMCU file refName = do
  res <- runX (readDocument [] file >>> mcu refName)
  case res of
    []  -> return $ error $ "no mcu parsed from " ++ file
    [x] -> return x
    [c1, c2]   -> return c1 -- for H7 series we get two MCUs as it has two Cores (M7 & M4)
    _   -> error $ "multiple mcus parsed from " ++ file

value x = any (==x) $ map (++"_VALUE") clockSourceNames

getCSName = (getAttrValue "Name" >>> isA value)

clockSources = atTag "RefParameter" >>>
  proc x -> do
    cs <- arr parseSourceValue <<< getCSName -< x
    val <- arr read <<< att "DefaultValue" -< x
    returnA -< cs val

parseClockSources f = do
  res <- runX (readDocument [] f >>> clockSources)
  return $ nubBy (\a b -> clockSourceName a == clockSourceName b) res

parseSourceValue = parseSource . take 3
parseSource "HSE" = HSE
parseSource "HSI" = HSI
parseSource "MSI" = MSI
parseSource "LSE" = LSE
parseSource "LSI" = LSI

afs = atTag "GPIO_Pin" >>>
  proc x -> do
    pin <- (arr $ parsePin . drop 1) <<< isA ((=='P') . head) <<< att "Name" -< x
    sigs <- isA (not . null) <<< listA pinSignal -< x
    returnA -< (pin, nub sigs)
  where
    parsePin :: String -> (String, Int)
    parsePin x = (takeWhile Char.isLetter x, read $ takeWhile Char.isDigit $ dropWhile Char.isLetter x)

pinSignal = atTag "PinSignal" >>> atTag "SpecificParameter" >>>
  proc x -> do
    (getAttrValue "Name" >>> isA (=="GPIO_AF")) -< x
    pv <- (arr $ parseAf .  drop 5)
       <<< isA (not . ("AFIO" `isInfixOf`)) -- AFIO is F1 specific
       <<< isA (not . ("EVENTOUT" `isSuffixOf`))
       <<< textAtTag "PossibleValue" -< x
    returnA -< pv
  where
    parseAf :: String -> (String, Int)
    parseAf x = (drop 1 $ dropWhile (/='_') x, read $ takeWhile (/='_') $ dropWhile Char.isLetter x)

parseAFs f = do
  res <- runX (readDocument [] f >>> afs)
  return $ groupAFsByPort res

groupAFsByPort afs =
  let ports = nub $ map (fst . fst) afs
      portPins x = map (\((_port, pin), xs) -> (pin, xs)) $ filter ((==x) . fst . fst) afs
  in map (\p -> (p, portPins p)) ports
