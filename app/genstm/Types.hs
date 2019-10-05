{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Turtle
import Data.Maybe
import Data.Either
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B

import Control.Monad.Reader

import Data.Ivory -- .Pretty (replace)
import Data.SVD hiding (svd, ppPeripheral)
import Data.CMX
import Data.STM32

import Coerce
import Utils

import Text.Regex.Posix

data DB = DB {
    cmxs :: M.Map Family [MCU]
  , cmxsWithSVD :: M.Map Family [MCU]
  , noSVD :: [MCU]
  , svds :: [(String, Device)]
  , devNames :: [STM32DevName]
  , shortDevNames :: [String]
  , nameMapped :: M.Map STM32DevName MCU
  } deriving (Show)

type MonadGen a = ReaderT DB IO a

runGen gen = do
  db <- loadDatabases
  runReaderT gen db

get :: String -> MonadGen Device
get x = do
  svds' <- svds <$> ask
  return $ getSVD svds' x

svdForMCU :: MCU -> MonadGen Device
svdForMCU = get . mcuRefName

memMap :: MCU -> MonadGen [(String, String)]
memMap mcu = svdForMCU mcu >>= return . getDevMemMap

loadDatabases = do
  mPath <- need "DB_PATH"
  dbPath <- case mPath of
    Nothing -> die "need DB_PATH env var"
    Just p -> return $ fromText p
  svds <- fmap fixSVDs $ extractSVDCached dbPath
  cmxs <- extractCMXCached dbPath
  let
    supp = filterSupported cmxs
    cmxsWithSVD = filterHavingSVD (supp, svds)
    noSVD = (cmxDevices supp L.\\ cmxDevices cmxsWithSVD)
    extract (Left x) = error $ "Unable to parse name" ++ x
    extract (Right x) = x
    nameMapped = M.fromList
               $ map (\d -> (extract $ parseName $ B.pack $ mcuRefName d, d))
               $ cmxDevices
               $ cmxsWithSVD

    devNames = M.keys nameMapped

    shortDevNames = S.toList $ S.fromList $ map shortName devNames

  return DB{..}

dbStats :: MonadGen ()
dbStats = do
  DB{..} <- ask
  let hasSVD = length $ catMaybes $ map (getSVDMaybe svds . mcuRefName) (cmxDevices cmxs)
      tot = length $ cmxDevices cmxs
      fams = length $ cmxFamilies cmxs
      supFams = length $ cmxFamilies $ filterSupported cmxs
  liftIO $ putStrLn $ unlines [
      "Families " ++ (show $ fams)
    , "Supported families " ++ (show $ supFams) ++ " " ++ (show supportedFamilies)
    , "Un-supported families " ++ (show $ fams - supFams) ++ " " ++ (show (cmxFamilies cmxs L.\\ supportedFamilies))
    , "CMX devices " ++ (show $ tot)
    , "SVD files " ++ (show $ length $ svds)
    , "CMX devices with SVD files " ++ (show $ hasSVD)
    , "CMX devices missing SVD files " ++ (show $ tot - hasSVD)
    , "SVD file names " ++ (show $ map fst svds)
    , "Supported but missing SVD files "
    ] ++ unlines (map mcuRefName noSVD)

filterHavingSVD (cmxs, svds) = M.map (filter (isJust . getSVDMaybe svds . mcuRefName)) cmxs

-- svd names look like STM32F41x, we replace x with [0-9A] so we can regex match on them
-- prefer longer pattern
-- (A in patter match due to L4A6 which hopefully matches L4x6.svd)
svdByMcu svds m = reverse
           $ L.sortBy (comparing fst)
           $ filter (\(name, dev) ->
                        (m =~ (replace "x" "[0-9A]" $ take 4 name)) :: Bool) svds
getSVDMaybe svds m = case svdByMcu svds m of
           [] -> Nothing
           x:xs -> Just $ x


getSVD :: [(String, Device)] -> String -> Device
getSVD svds = snd . fromJust . getSVDMaybe svds

processedPeriph :: Periph -> MCU -> MonadGen Peripheral
processedPeriph periph mcu = do
  dev <- svdForMCU mcu
  let mdi = mcuPeriphDriver mcu periph
  procPeriph periph (maybe Nothing diVersion mdi) (getPeriph (show periph) dev)
    --Nothing ->  error $ "Multiple or no drivers found for periph and mcu: " ++ show periph ++ ", " ++ mcuRefName mcu

svdsFamily :: Family -> MonadGen [Device]
svdsFamily f = do
  DB{..} <- ask
  mapM (get . mcuRefName) $ fromJust $ M.lookup f cmxsWithSVD

log :: String -> MonadGen ()
log = liftIO . putStrLn

askSVDs xs = runGen $ do
  db <- ask
  mapM (\x -> svdForMCU $ head $ fx (cmxs db) x) xs
