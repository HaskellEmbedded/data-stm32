module {{ modns }}
  ( MCU(..)
  , defMCU
  , matchMCU
  )
  where

import Data.Char (toUpper)
import System.Environment
import Text.Regex.Posix

import Ivory.BSP.STM32.Family

data MCU = MCU {
    mcuRAM    :: Integer
  , mcuRAM2   :: Maybe Integer -- for L4s
  , mcuROM    :: Integer
  , mcuCCM    :: Maybe Integer
  , mcuEEP    :: Maybe Integer
  , mcuName   :: String
  , mcuMatch  :: String
  , mcuFamily :: Family
  } deriving (Eq, Show)

defMCU :: MCU
defMCU = MCU {
    mcuRAM    = 0
  , mcuRAM2   = Nothing
  , mcuROM    = 0
  , mcuCCM    = Nothing
  , mcuEEP    = Nothing
  , mcuName   = ""
  , mcuMatch  = ""
  , mcuFamily = W
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> putStrLn "invalid argument\nUSAGE: lal MCU"
    [m] -> print $ matchMCU m
    _   -> putStrLn "Need MCU match"

-- match string against a list of (pattern, MCU)
matchMCU :: String -> Maybe MCU
matchMCU name = case filter (\(pat, _dev) -> (map toUpper name) =~ pat :: Bool) devices of
  [] -> Nothing
  [(matched, mcu)] -> Just $ mcu { mcuName = name
                                 , mcuMatch = matched
                                 , mcuFamily = nameToFamily name
                                 }

  _ -> fail "Multiple MCUs matched"

kbytes :: Integer -> Integer
kbytes = (*1024)

devices :: [(String, MCU)]
{{ devices }}
