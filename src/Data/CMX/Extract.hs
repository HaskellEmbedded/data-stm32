{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Extract (
    cmxFamilies
  , cmxDevices
  , extractCMX
  , extractCMXCached
  , filterSupported
  , uniqueIpNames
  , uniqueIpVersions
  , uniqueIps
  , hasIp
  , hasIpVersion
  , mcusWithIp
  , ipPerMCUCounts
  , ipFamilies
  , ipShortName
  , getIPVersion
  , fx
  ) where

import Turtle hiding (x)
import Prelude hiding (FilePath)
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe
import qualified Data.Ord
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import Data.Default.Class
import Data.Serialize
import Data.Char (toUpper)

import Data.STM32.Types (Family(..), supportedFamilies)
import Data.STM32.Name
import Data.STM32.Memory
import Data.STM32.Periph
import Data.CMX.Types
import Data.CMX.Parse

cmxFamilies :: M.Map k a -> [k]
cmxFamilies = M.keys
cmxDevices :: M.Map k [a] -> [a]
cmxDevices  = concat . M.elems

filterSupported :: M.Map Family [MCU] -> M.Map Family [MCU]
filterSupported = M.filterWithKey (\k _ -> k `elem` supportedFamilies)

extractCMX :: FilePath -> IO (M.Map Family [MCU], M.Map String AlternateFunctions)
extractCMX dbPath = do
  fs <- parseFamilies (dbPath ++ "/db/mcu/families.xml")
  xs <- forM (M.toList . M.map (concatMap subFamMCUs) $ fs) $ \(fam, devs) -> do
    ds <- forM devs $ \dev -> do
      putStrLn $ "Parsing " ++ smcuName dev
      mcu <- parseMCU (dbPath ++ "/db/mcu/" ++ smcuName dev ++ ".xml") (smcuRefName dev)
      let m = checkMCU $ fixMCU $ mcu {
                mcuRam = smcuRam dev
              , mcuFlash = smcuFlash dev
              }

          name = extract $ parseName (B.pack $ mcuRefName mcu)
          extract (Left x) = error $ "Unable to parse name" ++ x
          extract (Right x) = x

      cs <- parseClockSources $ ipXMLPath dbPath RCC m

      return $ snd $ adjustRAMs (name, m { mcuClocks = cs })

    return (fam, ds)

  afModes <- extractAlternateFunctions dbPath (concatMap snd xs)
  return (M.fromList xs, afModes)

extractAlternateFunctions :: FilePath -> [MCU] -> IO (M.Map String AlternateFunctions)
extractAlternateFunctions dbPath mcus = do
  let uniqueGPIOModes = L.nub $ map (\m -> (ipShortName GPIO m, ipXMLPath dbPath GPIO m)) mcus
  x <- forM uniqueGPIOModes $ \(name, path) -> do
    afs <- parseAFs path
    return (name, afs)

  return $ M.fromList x

extractCMXCached :: FilePath -> IO (M.Map Family [MCU], M.Map String AlternateFunctions)

extractCMXCached dbPath = do
  hasCache <- testfile cachePath
  if hasCache
    then
    (do
      putStrLn $ "Loading cached CubeMX database from " ++ cachePath
      cached <- B.readFile cachePath
      case decode cached of
        Left em -> fail em
        Right cmx -> return cmx)

    else
    (do
      putStrLn $ "Parsing CubeMX database from " ++ dbPath
      x <- extractCMX dbPath
      putStrLn $ "Saving cache to " ++ cachePath
      B.writeFile cachePath (encode x)
      return x)
  where
    cachePath = "/tmp/data_stm32_cmx_cache"


fixMCU x@MCU{..} = x { mcuIps = addSYSCFG . addAFIO . addEXTI . coerceIPVersions . filterIps $ mcuIps }
  where
    coerceIPVersions = S.map (\ip -> ip { ipVersion = dropCMXSuffix $ ipVersion ip })
    dropCMXSuffix x | "_Cube" `L.isSuffixOf` x = take (length x - length ("_Cube" :: String)) x
    dropCMXSuffix x = x
    filterIps = S.filter (\IP{..} ->
      map toUpper ipName
      `notElem`
          [ "CORTEX-M7"
          , "GRAPHICS"
          , "GFXSIMULATOR"
          , "FATFS"
          , "FREERTOS"
          , "LWIP"
          , "LIBJPEG"
          , "NVIC"
          , "MBEDTLS"
          , "OPENAMP"
          , "PDM2PCM"
          , "TOUCHSENSING"
          , "USB_DEVICE"
          , "USB_HOST"
          ])

    addEXTI :: S.Set IP -> S.Set IP
    addEXTI = S.insert $ extiIP $ case mcuFamily of
      G0 -> "exti_g0"
      _  -> "exti_common"

    extiIP ver = IP {
        ipName = "EXTI"
      , ipVersion = ver
      , ipConfigFile = ""
      , ipClockEnableMode = ""
      , ipInstanceName = ""
      }

    addAFIO :: S.Set IP -> S.Set IP
    addAFIO s = case mcuFamily of
      F1 -> S.insert (def { ipName = "AFIO" }) s
      _  -> s

    addSYSCFG :: S.Set IP -> S.Set IP
    addSYSCFG = S.insert (def { ipName = "SYSCFG" })

-- fill in missing ram2 and ram3 sizes, compute real sram1 size
adjustRAMs :: (STM32DevName, MCU) -> (STM32DevName, MCU)
adjustRAMs nmcu@(name, mcu) = checkRAM $ calcRam1 $ addCcmRam (name, mcu {
    mcuRam2 = ram2Size nmcu
  , mcuRam3 = ram3Size nmcu
  })

-- compute ram1 size - we substract ram2 & ram3 & ccram from
-- total size
calcRam1 :: (STM32DevName, MCU) -> (STM32DevName, MCU)
calcRam1 (name, m@MCU{..}) = (name, m { mcuRam1 = ram1 })
  where
    total = mcuRam
    ram1 = total - (ram2 + ram3 + ccram)
    ram2 = Data.Maybe.fromMaybe 0 mcuRam2
    ram3 = Data.Maybe.fromMaybe 0 mcuRam3
    ccram = Data.Maybe.fromMaybe 0 mcuCcmRam

-- for some MCUs we don't have CCM RAM size information in cubemx files
-- so we add it manually based on `Data.STM32.Memory.ccmSize`
addCcmRam :: (STM32DevName, MCU) -> (STM32DevName, MCU)
addCcmRam nmcu@(name, mcu) = (name, maybeSetCcm mcu (ccmSize nmcu))
  where
    maybeSetCcm mcu' (Just _size) = mcu' { mcuCcmRam = ccmSize nmcu }
    maybeSetCcm mcu' Nothing = mcu'

checkRAM :: (STM32DevName, MCU) -> (STM32DevName, MCU)
checkRAM nmcu@(_name, MCU{..})
  | mcuRam ==
      mcuRam1
    + Data.Maybe.fromMaybe 0 mcuRam2
    + Data.Maybe.fromMaybe 0 mcuRam3
    + Data.Maybe.fromMaybe 0 mcuCcmRam = nmcu
checkRAM (name, _)
  = error $ "ram1 + ram2 + ram3 + ccram is not equal mcuRam" ++ showName name

checkMCU :: MCU -> MCU
checkMCU MCU{..} | mcuFlash == 0 && mcuFamily /= MP1 = error $ "MCU Flash is 0 @" ++ mcuRefName
checkMCU MCU{..} | mcuRam   == 0 = error $ "MCU Ram is 0 @" ++ mcuRefName
checkMCU x = x

uniqueIps :: M.Map Family [MCU] -> S.Set (String, String)
uniqueIps = S.fromList
          . concatMap (map (\IP{..} -> (ipName, ipVersion))  . S.toList . mcuIps)
          . cmxDevices

uniqueIpNames :: M.Map Family [MCU] -> S.Set String
uniqueIpNames = S.map fst . uniqueIps

uniqueIpVersions :: M.Map Family [MCU] -> S.Set String
uniqueIpVersions = S.map snd . uniqueIps

hasIp :: (String, String) -> MCU -> Bool
hasIp (ipname, ipversion) = not . S.null
                          . S.filter (\ip -> ipName ip == ipname
                                          && ipVersion ip == ipversion)
                          . mcuIps
hasIpVersion :: String -> MCU -> Bool
hasIpVersion ipversion = not . S.null
                       . S.filter (\ip -> ipVersion ip == ipversion)
                       . mcuIps

mcusWithIp :: (String, String) -> M.Map Family [MCU] -> [MCU]
mcusWithIp p = filter (hasIp p) . cmxDevices

-- returns ip with the count of devices which use it
ipPerMCUCounts :: M.Map Family [MCU] -> [((String, String), Int)]
ipPerMCUCounts devs = L.sortOn (Data.Ord.Down . snd)
                    $ map (\p -> (p, length $ mcusWithIp p devs))
                    $ S.toList (uniqueIps devs)

-- returns ip with the list of families which use it
ipFamilies :: M.Map Family [MCU] -> [((String, String), [Family])]
ipFamilies devs = map (\ip -> (ip, L.nub $ map mcuFamily $ mcusWithIp ip devs))
                $ S.toList $ uniqueIps devs

-- filter devices starting with this prefix
-- fx (cmxs db) "F405"
fx :: M.Map Family [MCU] -> String -> [MCU]
fx db x = filter (\m -> ("STM32" ++ x) `L.isPrefixOf` mcuRefName m) . cmxDevices $ db

getIPVersion :: Periph -> MCU -> String
getIPVersion p m = case S.toList $ S.map ipVersion $ S.filter ((== show p) . ipName) $ mcuIps m of
  [] -> error $ "No IP found with name " ++ show p
  [x] -> x
  xs -> error $ "Multiple IPs found with name " ++ show p ++ " found " ++ show xs

-- STM32F091_gpio_v1_0 -> F091
ipShortName :: Periph -> MCU -> String
ipShortName periph mcu = takeWhile (/='_') . drop 5 $ getIPVersion periph mcu

-- | Get a path to peripherals IP description xml
ipXMLPath :: FilePath -> Periph -> MCU -> String
ipXMLPath dbPath periph mcu = concat
  [ dbPath
  , "/db/mcu/IP/"
  , show periph
  , "-"
  , getIPVersion periph mcu
  , "_Modes.xml" ]
