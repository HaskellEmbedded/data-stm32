{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Extract where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import Data.CMX.Types
import Data.CMX.Parse

cmxFamilies fs = M.keys

cmxDevices fs = concat
  $ M.elems
  $ M.map (concatMap (\x -> subFamMCUs x)) fs

cmxDevicesRefNames :: Families -> [String]
cmxDevicesRefNames = map smcuRefName . cmxDevices

cmxDevicesXMLNames :: Families -> [String]
cmxDevicesXMLNames = map smcuName . cmxDevices

lol = do
  let dbPath = "data"
  fs <- parseFamilies (dbPath ++ "/db/mcu/families.xml")
  p <- forM (take 1 $ cmxDevices fs) $ \dev -> do
    mcu <- parseMCU $ dbPath ++ "/db/mcu/" ++ (smcuName dev) ++ ".xml"
    return $ checkMCU $ fixMCU $ mcu {
        mcuRam = smcuRam dev
      , mcuFlash = smcuFlash dev
      }
    -- F0 [ShortMCU MCU]
  -- print $ length p
  return p

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
