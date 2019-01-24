{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.MemMap where

import Control.Monad
import System.Exit

import Data.SVD

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

data Core = CortexM0 | CortexM0Plus | CortexM3 | CortexM4F | CortexM7F
  deriving (Eq, Show)

data Family = F0 | F1 | F2 | F3 | F4 | F7 | H7 | L0 | L1 | L4 | W | T
  deriving (Eq, Show)

periphBase :: Int
periphBase = 0x40000000

-- flash base 0x08000000
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

core :: Family -> Core
core F0 = CortexM0
core F1 = CortexM3
core F2 = CortexM3
core F3 = CortexM4F
core F7 = CortexM7F
core H7 = CortexM7F
core L0 = CortexM0Plus
core L1 = CortexM3
core L4 = CortexM4F
core W = CortexM3
core T = CortexM3

fpu :: Core -> String
fpu CortexM0 = "soft"
fpu CortexM0Plus = "soft"
fpu CortexM3 = "soft"
fpu CortexM4F = "fpv4-sp-d16"
fpu CortexM7F = "fpv5-sp-d16"

eabi :: Core -> Maybe String
eabi CortexM4F = Just "hard"
eabi CortexM7F = Just "hard"
eabi _ = Nothing

ramOffset :: Family -> Int
ramOffset F7 = 0x20010000
ramOffset _  = 0x20000000

ccmOffset :: Family -> Maybe Int
ccmOffset F3 = Just 0x10000000
ccmOffset F4 = Just 0x10000000
ccmOffset F7 = Just 0x20000000
ccmOffset _  = Nothing
