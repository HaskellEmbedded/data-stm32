{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RamSpec where

import System.Environment
import Filesystem.Path.CurrentOS

import SpecHelper

dbPath = do
  p <- getEnv "DB_PATH"
  return (decodeString p)

withCMX' = do
  p <- dbPath
  extractCMXCached p

withCMX = beforeAll $ fst <$> withCMX'

find devs x = case fx devs x of
  (x:xs) -> x
  _ -> error "No device found"

itsRam which ap x size = do
  it (x ++ " has size " ++ show size) $ \ds -> do
    (which $ find ds x) `shouldBe` (ap $ size * 1024)

itsRam1 = itsRam mcuRam1   id
itsRam2 = itsRam mcuRam2   Just
itsRam3 = itsRam mcuRam3   Just
itsCcm  = itsRam mcuCcmRam Just

itHasNoRam2 x = itsRam mcuRam2   (pure Nothing) x 0
itHasNoRam3 x = itsRam mcuRam3   (pure Nothing) x 0
itHasNoCcm  x = itsRam mcuCcmRam (pure Nothing) x 0

toNamed x = (mcuName x, x)

itsContinuousRam x cr =
  it (x ++ " has continuos ram: " ++ show cr) $ \ds -> do
    (ramContinuos $ rams $ toNamed $ find ds x) `shouldBe` cr

spec :: Spec
spec = withCMX $ do
  itsRam1 "F103" 6

  itsRam1 "F334" 12
  itsCcm  "F334" 4

  itsRam1 "F405" 112
  itsRam2 "F405" 16
  itsCcm  "F405" 64
  itHasNoRam3 "F405"

  itsRam1 "L431" 32
  itsRam2 "L431" 32
  itHasNoRam3 "L431"

  itsRam1 "G474" 48
  itsRam2 "G474" 16
  itHasNoRam3 "G474"
  itsCcm "G474" 32

  itsRam1 "F765" 368
  itsRam2 "F765" 16
  itHasNoRam3 "F765"
  itsCcm "F765" 128

  itsContinuousRam "F405" True
  itsContinuousRam "L475" False
  itsContinuousRam "L476" False
  itsContinuousRam "L486" False

main :: IO ()
main = hspec spec
