{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Extract where

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Data.ByteString.Char8 as B
import Data.Ord (comparing)
import qualified Data.List as L
import qualified Data.Map as Map

import Text.Regex.Posix

import Data.Serialize

import Data.SVD hiding (svd, ppPeripheral)
--import Data.CMX
import Debug.Trace

import Utils

-- parse SVD files into ("STM32F031x", Device {..} )
svd :: (String -> Bool) -> IO [(String, Device)]
svd devFilter = do
  dir <- pwd

  hasCache <- testfile "svd_cache"
  case hasCache of
    True -> do
      echo "Loading SVDs from cache"
      cached <- B.readFile "svd_cache"
      case decode cached of
        Left err -> fail err
        Right svds -> return svds

    False -> do
      echo "Parsing SVDs"

      cd dir
      cd "STMicro"
      svdxmls <- fold (onFiles (grepText (prefix "./STM32" <> suffix ".svd")) (ls ".")) Fold.list
      svds <- mapM svd1 $ filter devFilter $ map fpToString svdxmls

      cd dir
      B.writeFile "svd_cache" (encode svds)
      return svds

svd1 f = do
  res <- parseSVD f
  case res of
    Left e -> do
      putStrLn ("No parse of " ++ f ++ " error was " ++ e)
      exitFailure
    Right x -> do
      return $ (drop 5 $ deviceName x, x)

cmx :: IO ([String], [String], [String])
cmx = do
  cd "db"
  cd "mcu"
  mcuxmls <- fold (onFiles (grepText (prefix "./STM32" <> suffix ".xml")) (ls ".")) Fold.list
  let shortMCUs = L.sort $ L.nub $ map (take 4 . drop 7 . fpToString) mcuxmls
  let families  = L.sort $ L.nub $ map (take 2 . drop 7 . fpToString) mcuxmls
  return (shortMCUs, families, map fpToString mcuxmls)

-- extract all registers for peripheral `periph`
byPeriphRegs :: String -> (a, Device) -> [Register]
byPeriphRegs periph dev = case ((filter ((== periph) . periphGroupName)) . devicePeripherals . snd $ dev) of
  [] -> trace ("Device " ++ (deviceName . snd $ dev) ++ " has no " ++ periph) []
  x -> periphRegisters . head $ x

-- get merged register map (containing regs for all families) for specific peripheral
mergedRegistersForPeriph p svds = (getPeriph p (snd fstDev)) { periphRegisters = merged }
  where
      groups = L.groupBy ((==) `on` byPeriphRegs p) svds

      merged = mergeRegisterFields $ concatMap (byPeriphRegs p) svds

      fstDev = head $ svds
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
    mf rs = (head rs) { regFields = mergedFields rs }
    mergedFields rs = Map.elems $ Map.unionsWith preferred (map toMap rs)
    toMap r = Map.fromList (map (\f -> (fieldBitOffset f, f)) (regFields r))

    -- if fields collide we use prefered names from here
    preferences = [ "DIV_Fraction" ]

    preferred x y | fieldName x `elem` preferences = x
    preferred _ y = y
