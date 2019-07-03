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
  , mcusWithIp
  , ipPerMCUCounts
  , ipFamilies
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
import Data.CMX.Types
import Data.CMX.Parse

cmxFamilies = M.keys
cmxDevices  = concat . M.elems

filterSupported :: M.Map Family [MCU] -> M.Map Family [MCU]
filterSupported = M.filterWithKey (\k a -> k `elem` supportedFamilies)

extractCMX :: FilePath -> IO (M.Map Family [MCU])
extractCMX dbPath = do
  fs <- parseFamilies (encodeString dbPath ++ "/db/mcu/families.xml")
  p <- forM (M.toList . M.map (concatMap subFamMCUs) $ fs) $ \(fam, devs) -> do
    ds <- forM devs $ \dev -> do
      mcu <- parseMCU $ encodeString dbPath ++ "/db/mcu/" ++ (smcuName dev) ++ ".xml"
      return $ checkMCU $ fixMCU $ mcu {
          mcuRam = smcuRam dev
        , mcuFlash = smcuFlash dev
        , mcuRefName = smcuRefName dev
        }

    return (fam, ds)
  return $ M.fromList p

extractCMXCached :: FilePath -> IO (M.Map Family [MCU])
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
fx :: M.Map Family [MCU] -> String -> [MCU]
fx db x = filter (\m -> x `L.isPrefixOf` (mcuRefName m)) . cmxDevices $ db
