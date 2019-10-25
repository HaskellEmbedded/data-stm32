{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Prelude hiding (log)
import Turtle
import Data.Maybe
import Data.Either
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Text.Pretty.Simple


-- functor
import Data.Algorithm.Diff
import Control.Monad.Reader

import Data.Ivory -- .Pretty (replace)
import Data.SVD hiding (svd, ppPeripheral)
import Data.CMX
import Data.STM32

import Coerce
import Utils

import Text.Regex.Posix
import Options.Applicative


data DB = DB {
    cmxs :: M.Map Family [MCU]
  , cmxsWithSVD :: M.Map Family [MCU]
  , afs :: M.Map String AlternateFunctions
  , noSVD :: [MCU]
  , svds :: [(String, Device)]
  , devNames :: [STM32DevName]
  , shortDevNames :: [String]
  , nameMapped :: M.Map STM32DevName MCU
  , opts :: Options
  } deriving (Show)

data Options = Options {
    exclude :: [String]
  , include :: [String]
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
  (cmxs, afs) <- extractCMXCached dbPath
  opts <- execParser $ info (parseOptions <**> helper) (fullDesc <> progDesc "genstm")
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


peripheralByVersion :: Periph -> String -> MonadGen (Peripheral)
peripheralByVersion p ver = do
  db <- ask
  case filter (hasIpVersion ver) (cmxDevices $ cmxsWithSVD db) of
    [] -> error $ "No peripheral found for IP version " ++ ver
    (x:_) -> do
      svd <- get $ mcuRefName x
      return $ getPeriph (show p) svd

periphVersions :: Periph -> MonadGen ([String])
periphVersions p = do
  db <- ask
  return $ map snd $ filter ((==show p) . fst) $ S.toList $ uniqueIps $ cmxsWithSVD db

lp x = liftIO $ pPrint $ x

diffPeriphs :: Periph -> MonadGen ()
diffPeriphs p = do
  vers <- periphVersions p
  forM_ (zip vers (tail vers)) $ \(v1, v2) -> do
    x <- peripheralByVersion p v1
    y <- peripheralByVersion p v2
    liftIO $ print (v1, v2)
    let dr = diffRegNames x y
    log $ renderDiff $ dr
    lp $ diffDistance $ diffRegNames x y
    forM_ (getBoths dr) $ \rName -> do
      log $ renderDiff $ map (fmap shortField) $ diffFields (regNameFields rName x) (regNameFields rName y)
      lp $ diffDistance $ diffFields (regNameFields rName x) (regNameFields rName y)

instance Functor Diff where
  fmap f (Both x y) = (Both (f x) (f y))
  fmap f (First x) = (First (f x))
  fmap f (Second x) = (Second (f x))

parseOptions :: Parser Options
parseOptions = Options <$>
    many (strOption (
               long "exclude"
            <> short 'e'
            <> metavar "MCU"
            <> help "Exclude MCU(s) from output"))
  <*>
    many (strOption (
               long "include"
            <> short 'i'
            <> metavar "MCU"
            <> help "Only process these MCU(s)"))

devFilter Options{include=[], exclude = []} sn = True
devFilter Options{include=is, exclude = []} sn = elem sn is
devFilter Options{include=[], exclude = es} sn = not $ elem sn es
devFilter _ _ = error "Cannot use both include and exclude"

filteredDevs ::  MonadGen ([(String, MCU)])
filteredDevs = do
  db <- ask
  return $ filter (devFilter (opts db) . fst) $ M.toList $ M.mapKeys shortName $ nameMapped db

filteredShortNames ::  MonadGen ([String])
filteredShortNames = do
  db <- ask
  return $ filter (devFilter (opts db)) $ shortDevNames db
