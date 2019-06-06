{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Extract (
    cmxFamilies
  , cmxDevices
  , extractCMX
  , extractCMXCached
  , filterSupported
  ) where

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import Data.Serialize

import Data.STM32.Types (Family(..), supportedFamilies)
import Data.CMX.Types
import Data.CMX.Parse

cmxFamilies = M.keys
cmxDevices  = concat . M.elems

filterSupported :: M.Map Family [MCU] -> M.Map Family [MCU]
filterSupported = M.filterWithKey (\k a -> k `elem` supportedFamilies)

extractCMX :: String -> IO (M.Map Family [MCU])
extractCMX dbPath = do
  fs <- parseFamilies (dbPath ++ "/db/mcu/families.xml")
  p <- forM (M.toList . M.map (concatMap subFamMCUs) $ fs) $ \(fam, devs) -> do
    ds <- forM devs $ \dev -> do
      mcu <- parseMCU $ dbPath ++ "/db/mcu/" ++ (smcuName dev) ++ ".xml"
      return $ checkMCU $ fixMCU $ mcu {
          mcuRam = smcuRam dev
        , mcuFlash = smcuFlash dev
        }

    return (fam, ds)
  return $ M.fromList p

extractCMXCached :: String -> IO (M.Map Family [MCU])
extractCMXCached dbPath = do
  hasCache <- testfile $ decodeString $ cachePath
  case hasCache of
    True -> do
      putStrLn $ "Loading cached CubeMX database from " ++ (show cachePath)
      cached <- B.readFile cachePath
      case decode cached of
        Left err -> fail err
        Right cmx -> return cmx

    False -> do
      putStrLn $ "Parsing CubeMX database from " ++ (show dbPath)
      x <- extractCMX dbPath
      putStrLn $ "Saving cache to " ++ (show cachePath)
      B.writeFile cachePath (encode x)
      return x
  where
    cachePath = "/tmp/data_stm32_cmx_cache"


fixMCU x@MCU{..} = x { mcuIps = filterIps mcuIps }
  where
    filterIps = S.filter (\IP{..} ->
      not $ elem ipName [ "MBEDTLS"
                        , "CORTEX-M7"
                        , "GRAPHICS"
                        , "GFXSIMULATOR"
                        , "FATFS"
                        , "LWIP"
                        , "LIBJPEG"
                        , "NVIC"
                        , "TOUCHSENSING"
                        , "PDM2PCM"
                        , "FREERTOS"
                        , "USB_DEVICE"
                        , "USB_HOST"
                        ])

checkMCU MCU{..} | mcuFlash == 0 = error $ "MCU Flash is 0 @" ++ mcuRefName
checkMCU MCU{..} | mcuRam   == 0 = error $ "MCU Ram is 0 @" ++ mcuRefName
checkMCU x = x
