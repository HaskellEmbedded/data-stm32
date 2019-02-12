{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ivory where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ivory.Pretty
import Data.Ivory.MemMap

ibs = "Ivory.BSP."
--- XXX
tdir = "/home/rmarko/git/data-stm32/templates/"


procHSTemplate :: Text -> [(Text, Text)] -> IO (Text, Text)
procHSTemplate name attrs =  procTemplate name' attrs' >>= return . ((,) modns)
  where
    name' = T.concat [T.replace "." "/" name, ".hs"]
    attrs' = ("modns", modns):attrs
    modns = T.concat [ibs, name]

procFamilyTemplate :: Family -> Text -> [(Text, Text)] -> IO (Text, Text)
procFamilyTemplate family name attrs =  procTemplate name' attrs' >>= return . ((,) modns)
  where
    name' = T.concat [T.replace "." "/" name, ".hs"]
    attrs' = ("modns", modns):attrs
    modns = T.concat [ibs, T.replace "XX" (T.pack $ show family) name]

-- STM32.Peripheral.X.Regs to STM32.Peripheral.UART.Regs
-- opens X/Regs.hs
procPeriphTemplate :: Text -> Text -> Maybe Text -> [(Text, Text)] -> IO (Text, Text)
procPeriphTemplate periph name vers attrs =  procTemplate name' attrs' >>= return . ((,) modns)
  where
    name' = T.concat [T.replace "." "/" name, ".hs"]
    attrs' = ("modns", modns):attrs
    modns = case vers of
      Nothing -> T.concat [ibs, T.replace "X" periph name]
      Just x -> T.concat [ibs, T.replace "X" (periph <> x) name]

-- STM32.Peripheral.X.Peripheral to STM32.Peripheral.UART.Peripheral
-- opens UART/Regs.hs
procPeriphSpecificTemplate :: Text -> Text -> Maybe Text -> [(Text, Text)] -> IO (Text, Text)
procPeriphSpecificTemplate periph name vers attrs =  procTemplate name' attrs' >>= return . ((,) modns)
  where
    name' = T.concat [T.replace "X" periph $ T.replace "." "/" name, ".hs"]
    attrs' = ("modns", modns):attrs
    modns = case vers of
      Nothing -> T.concat [ibs, T.replace "X" periph name]
      Just x -> T.concat [ibs, T.replace "X" (periph <> x) name]

-- STM32DEV.File to STM32FXYZ.File
procDevTemplate :: String -> Text -> [(Text, Text)] -> IO (Text, Text)
procDevTemplate dev name attrs =  procTemplate name' attrs' >>= return . ((,) modns)
  where
    name' = T.concat ["STM32DEV/", T.replace "." "/" name, ".hs"]
    attrs' = ("modns", modns):attrs
    modns = T.concat [ibs, T.replace "DEV" (T.pack dev) (T.concat ["STM32DEV.", name]) ]

procTemplate :: Text -> [(Text, Text)] -> IO Text
procTemplate name attrs = do
  t <- TIO.readFile $ T.unpack $ T.concat [tdir, name]
  return $ foldl (\c (what, with) -> T.replace (T.concat ["@", what, "@"]) with c) t attrs
