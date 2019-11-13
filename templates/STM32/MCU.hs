module {{ modns }}
  ( MCU(..)
  , STM32DevName(..)
  , STM32Config(..)
  , NamedMCU
  , matchMCU
  , testMatch
  , mcuNameParser
  , mcuConfigParser
  )
  where

import System.Environment
import System.FilePath.Posix

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Serialize

import qualified Paths_ivory_bsp_stm32 as P

import Ivory.BSP.STM32.ClockConfig
import Ivory.Tower.Config

import Data.CMX.Types
import Data.STM32.Types

data STM32Config = STM32Config {
    confMCU    :: NamedMCU
  , confClocks :: ClockConfig
  }

testMatch :: IO ()
testMatch = do
  args <- getArgs
  case args of
    []  -> putStrLn "invalid argument\nUSAGE: lal MCU"
    [m] -> matchMCU m >>= print . fst
    _   -> putStrLn "No MCU matched"

-- match string against a list of (pattern, MCU)
matchMCU :: String -> IO (STM32DevName, MCU)
matchMCU name = do
  db <- devices
  case findBest name (M.keys db) of
    [] -> fail "No MCU matched"
    [x] -> case M.lookup x db of
            Just mcu -> return (x, mcu)
            Nothing -> fail "Unable to lookup MCU"
    xs -> fail $ "Multiple MCUs matched: " ++ (unwords $ map showName xs)

devices :: IO (M.Map STM32DevName MCU)
devices = do
  d <- P.getDataDir
  db <- B.readFile (d </> "devices.data")
  case decode db of
    Left err -> fail err
    Right dec -> return dec

mcuNameParser :: String -> ConfigParser String
mcuNameParser def = subsection "args" $ subsection "mcu" string <|> pure def

mcuConfigParser :: NamedMCU -> ConfigParser NamedMCU
mcuConfigParser (name, mcu) = do
  ram   <- fmap Just (subsection "args" $ subsection "ram" integer)   <|> pure Nothing
  flash <- fmap Just (subsection "args" $ subsection "flash" integer) <|> pure Nothing
  split <- subsection "args" $ subsection "splitmem" bool <|> pure False

  return (name, mcu {
      mcuRam = maybe (mcuRam mcu) (fromIntegral) ram
    , mcuFlash = maybe (mcuFlash mcu) (fromIntegral) flash
    , mcuForceSplit = split
    })


