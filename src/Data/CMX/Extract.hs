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

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import Data.Serialize
import Data.Char (toUpper)

import Data.STM32.Types (Family(..), supportedFamilies)
import Data.STM32.Name
import Data.STM32.Memory
import Data.STM32.Periph
import Data.CMX.Types
import Data.CMX.Parse

cmxFamilies = M.keys
cmxDevices  = concat . M.elems

filterSupported :: M.Map Family [MCU] -> M.Map Family [MCU]
filterSupported = M.filterWithKey (\k a -> k `elem` supportedFamilies)

extractCMX :: FilePath -> IO (M.Map Family [MCU], M.Map String AlternateFunctions)
extractCMX dbPath = do
  fs <- parseFamilies (encodeString dbPath ++ "/db/mcu/families.xml")
  xs <- forM (M.toList . M.map (concatMap subFamMCUs) $ fs) $ \(fam, devs) -> do
    ds <- forM devs $ \dev -> do
      putStrLn $ "Parsing " ++ smcuName dev
      mcu <- parseMCU $ encodeString dbPath ++ "/db/mcu/" ++ (smcuName dev) ++ ".xml"
      let m = checkMCU $ fixMCU $ mcu {
                mcuRam = smcuRam dev
              , mcuFlash = smcuFlash dev
              , mcuRefName = smcuRefName dev
              }

          name = extract $ parseName (B.pack $ mcuRefName mcu)
          extract (Left x) = error $ "Unable to parse name" ++ x
          extract (Right x) = x

      cs <- parseClockSources $ ipXMLPath dbPath RCC m

      return $ snd $ adjustRAMs (name, m { mcuClocks = cs })

    return (fam, ds)

  afModes <- extractAlternateFunctions dbPath (concatMap snd xs)
  return $ (M.fromList xs, afModes)

extractAlternateFunctions :: FilePath -> [MCU] -> IO (M.Map String AlternateFunctions)
extractAlternateFunctions dbPath mcus = do
  let uniqueGPIOModes = L.nub $ map (\m -> (ipShortName GPIO m, ipXMLPath dbPath GPIO m)) mcus
  x <- forM uniqueGPIOModes $ \(name, path) -> do
    afs <- parseAFs path
    return (name, afs)

  return $ M.fromList x

extractCMXCached :: FilePath -> IO (M.Map Family [MCU], M.Map String AlternateFunctions)

extractCMXCached dbPath = do
  hasCache <- testfile $ decodeString $ cachePath
  case hasCache of
    True -> do
      putStrLn $ "Loading cached CubeMX database from " ++ cachePath
      cached <- B.readFile cachePath
      case decode cached of
        Left err -> fail err
        Right cmx -> return cmx

    False -> do
      putStrLn $ "Parsing CubeMX database from " ++ (encodeString dbPath)
      x <- extractCMX dbPath
      putStrLn $ "Saving cache to " ++ cachePath
      B.writeFile cachePath (encode x)
      return x
  where
    cachePath = "/tmp/data_stm32_cmx_cache"


fixMCU x@MCU{..} = x { mcuIps = coerceIPVersions . filterIps $ mcuIps }
  where
    coerceIPVersions = S.map (\ip -> ip { ipVersion = dropCMXSuffix $ ipVersion ip })
    dropCMXSuffix x | "_Cube" `L.isSuffixOf` x = take (length x - length ("_Cube" :: String)) x
    dropCMXSuffix x = x
    filterIps = S.filter (\IP{..} ->
      not $ elem (map toUpper ipName)
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

-- fill in missing ram2 and ram3 sizes, compute real sram1 size
adjustRAMs nmcu@(name, mcu) = checkRAM $ calcRam1 $ g4CcmRam $ (name, mcu {
    mcuRam2 = ram2Size nmcu
  , mcuRam3 = ram3Size nmcu
  })

-- compute ram1 size - we substract ram2 & ram3 & ccram from
-- total size
calcRam1 (name, m@MCU{..}) = (name, m { mcuRam1 = ram1 })
  where
    total = mcuRam
    ram1 = total - (ram2 + ram3)
    ram2 = maybe 0 id mcuRam2
    ram3 = maybe 0 id mcuRam3

-- for G4s we don't have CCM RAM size information in svd files
-- so we add it manually
g4CcmRam nmcu@(name, mcu) = (name, maybeSetCcm $ mcu { mcuRam = mcuRam mcu - ccm })
  where
    ccm = maybe 0 id $ ccmSize nmcu
    maybeSetCcm mcu | ccm /=0   = mcu { mcuCcmRam = ccmSize nmcu }
    maybeSetCcm mcu | otherwise = mcu

checkRAM nmcu@(name, MCU{..}) | mcuRam == mcuRam1 + (maybe 0 id mcuRam2) + (maybe 0 id mcuRam3) = nmcu
checkRAM (name, _) | otherwise = error $ "ram1 + ram2 + ram3 is not equal mcuRam" ++ showName name

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
ipPerMCUCounts devs = reverse
                    $ L.sortOn snd
                    $ map (\p -> (p, length $ mcusWithIp p devs))
                    $ S.toList (uniqueIps $ devs)

-- returns ip with the list of families which use it
ipFamilies :: M.Map Family [MCU] -> [((String, String), [Family])]
ipFamilies devs = map (\ip -> (ip, L.nub $ map mcuFamily $ mcusWithIp ip devs))
                $ S.toList $ uniqueIps devs

-- filter devices starting with this prefix
-- fx (cmxs db) "F405"
fx :: M.Map Family [MCU] -> String -> [MCU]
fx db x = filter (\m -> ("STM32" ++ x) `L.isPrefixOf` (mcuRefName m)) . cmxDevices $ db

getIPVersion :: Periph -> MCU -> String
getIPVersion p m = case S.toList $ S.filter ((== show p) . ipName) $ mcuIps m of
  [] -> error $ "No IP found with name " ++ (show p)
  [x] -> ipVersion x
  xs -> error $ "Multiple IPs found with name " ++ (show p)

-- STM32F091_gpio_v1_0 -> F091
ipShortName :: Periph -> MCU -> String
ipShortName periph mcu = takeWhile (/='_') . drop 5 $ getIPVersion periph mcu

ipXMLPath :: FilePath -> Periph -> MCU -> String
ipXMLPath dbPath periph mcu = concat
  [ encodeString dbPath
  , "/db/mcu/IP/"
  , (show periph)
  , "-"
  , getIPVersion periph mcu
  , "_Modes.xml" ]
