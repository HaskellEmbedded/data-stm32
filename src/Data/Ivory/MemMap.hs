{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.MemMap where

import Control.Monad
import System.Exit

import Data.SVD
import Data.STM32.Types

memmap :: String -> IO ()
memmap f = do
  res <- parseSVD f
  case res of
    Left e -> do
      putStrLn ("No parse of " ++ f ++ " error was " ++ e)
      exitFailure
    Right dev -> do
      print $ getDevMemMap dev

periphNameFormat x = x

getDevMemMap Device{..} = map (liftM2 (,) (hexFormat . periphBaseAddress) (periphNameFormat . periphName)) devicePeripherals

ramOffset :: Family -> Int
ramOffset F7 = 0x20010000
ramOffset _  = 0x20000000

ccmOffset :: Family -> Maybe Int
ccmOffset F3 = Just 0x10000000
ccmOffset F4 = Just 0x10000000
ccmOffset F7 = Just 0x20000000
ccmOffset _  = Nothing

periphBase :: Int
periphBase = 0x40000000

flashOffset :: Integer
flashOffset = 0x08000000

-- info  base 0x1ffff000

bases F0 =
  [ ("apb1", periphBase)
  , ("ahb1", periphBase + 0x20000)
  , ("ahb2", periphBase + 0x08000000)
  ]

bases F1 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x18000)
  ]

bases F2 = bases F7
bases F3 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x20000)
  , ("ahb2", 0x48000000)
  , ("ahb3", 0x50000000)
  ]

bases F4 = bases F7
bases F7 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x20000)
  , ("ahb2", 0x50000000)
  , ("ahb3", 0x60000000)
  ]

bases L0 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x20000)
  ]
  -- ioport base 0x50000000

bases L1 = bases L0

bases L4 = bases L0 ++ [ ("ahb2", 0x48000000) ]
  -- fmc1 base 0x60000000
  -- fmc3 base 0x80000000
  -- qspi bank base 0x90000000
  -- fmc qspi base 0xA0000000

