{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Extract where

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Map as Map

import Text.Regex.Posix
import Debug.Trace
import Safe

import Data.SVD
import Utils

-- extract all registers for peripheral `periph`
byPeriphRegs :: String -> (a, Device) -> [Register]
byPeriphRegs periph dev = case ((filter ((== periph) . periphGroupName)) . devicePeripherals . snd $ dev) of
  [] -> trace ("Device " ++ (deviceName . snd $ dev) ++ " has no " ++ periph) []
  x -> periphRegisters . headNote "byPeriphRegs" $ x

-- get merged register map (containing regs for all families) for specific peripheral
mergedRegistersForPeriph p svds = (getPeriph p (snd fstDev)) { periphRegisters = merged }
  where
      groups = L.groupBy ((==) `on` byPeriphRegs p) svds

      merged = mergeRegisterFields $ concatMap (byPeriphRegs p) svds

      fstDev = headNote "mergedRegistersForPeriph fstDev" $ svds
      -- make a test of this (check if regName groups contain all devices)
      --wtf = L.groupBy (\a b -> regName a == regName b) $ L.sortBy (comparing $ regName) $ concatMap byPeriphRegs p svds
      --print $ map length wtf
      --
  --mapM_ (pPrint . byP . head ) groups
  --putStrLn $ SVD.ppPeripheral $ byP  $ head (groups !! 1)
  --putStrLn $ ppPeripheral periph


-- merge fields in respective registers grouped by register name.
--
-- used to create one register containing all fields from similar devices of a family
mergeRegisterFields :: [Register] -> [Register]
mergeRegisterFields rs = mergeRegFields $ L.groupBy ((==) `on` regName) $ L.sortBy (comparing $ regName) $ rs

mergeRegFields :: [[Register]] -> [Register]
mergeRegFields groups = map mf groups
  where
    mf :: [Register] -> Register
    mf rs = (headNote "mergeRegFields" rs) { regFields = mergedFields rs }
    mergedFields rs = Map.elems $ Map.unionsWith preferred (map toMap rs)
    toMap r = Map.fromList (map (\f -> (fieldBitOffset f, f)) (regFields r))

    -- if fields collide we use prefered names from here
    preferences = [ "DIV_Fraction" ]

    preferred x y | fieldName x `elem` preferences = x
    preferred _ y = y
