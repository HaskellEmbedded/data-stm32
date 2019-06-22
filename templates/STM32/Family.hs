
module {{ modns }}
  ( Family(..)
  , strToFamily
  , nameToFamily
  , flashOffset
  , ramOffset
  , ccmOffset
  ) where

--import Data.Char (toUpper)
--import Ivory.Tower.Config

data Family = F0 | F1 | F2 | F3 | F4 | F7 | H7 | L0 | L1 | L4 | L4Plus | W | T
  deriving (Eq, Show)

strToFamily :: String -> Family
strToFamily "F0" = F0
strToFamily "F1" = F1
strToFamily "F2" = F2
strToFamily "F3" = F3
strToFamily "F4" = F4
strToFamily "F7" = F7
strToFamily "H7" = H7
strToFamily "L0" = L0
strToFamily "L1" = L1
strToFamily "L4" = L4
strToFamily "L4+" = L4Plus
strToFamily "W"  = W
strToFamily "T"  = T
strToFamily x    = error $ "Unable to identify family by" ++ x

-- STM32XYZ to Family
nameToFamily :: String -> Family
nameToFamily = strToFamily . trim . (take 2) . (drop 5)
  where
    trim ('W':_) = "W"
    trim ('T':_) = "T"
    trim x = x

flashOffset :: Integer
flashOffset = 0x08000000

ramOffset :: Family -> Integer
ramOffset F7 = 0x20010000
ramOffset _  = 0x20000000

ccmOffset :: Family -> Integer
ccmOffset F3 = 0x10000000
ccmOffset F4 = 0x10000000
ccmOffset F7 = 0x20000000
ccmOffset _  = 0x0
