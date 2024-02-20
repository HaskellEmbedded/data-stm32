module Data.STM32.Util where

import Data.Serialize
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Data.CMX.Types
import Data.STM32.Types

-- Helper for reading serialized devices.data file
devices :: FilePath -> IO (M.Map STM32DevName MCU)
devices f = do
  db <- B.readFile f
  case decode db of
    Left err -> fail err
    Right dec -> return dec
