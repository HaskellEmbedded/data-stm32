{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.STM32.Family where

import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Serialize
import Data.Attoparsec.ByteString.Char8
import qualified Data.Maybe
import Data.Foldable

data Family =
    F0
  | F1
  | F2
  | F3
  | F4
  | F7
  | H5
  | H7
  | L0
  | L1
  | L4
  | L4Plus -- L4S L4R
  | L5
  | U5
  | C0
  | G0
  | G4
  | WB
  | WBA
  | WL
  | TS
  | MP1
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialize Family

showFam :: Family -> String
showFam L4Plus = "L4"
showFam x = show x

familyParser :: Parser Family
familyParser = do
  f <- asum
    [ MP1 <$ string "MP1"
    , G4  <$ string "GB"
    , (\a b ->
        Data.Maybe.fromMaybe
          (error $ "Cannot read family out of " ++ [a, b])
          $ readMaybe [a, b]) <$> anyChar <*> anyChar
    ]

  return f

-- | List of STM32 families supported by the generator
supportedFamilies :: [Family]
supportedFamilies =
  [ F0
  , F1
  , F3
  , F4
  , F7
  , G0
  , G4
  , L4
  , L4Plus
  ]

-- | Always ignore some problematic devices
deviceIgnoresShortNames :: [String]
deviceIgnoresShortNames = [
    "G061"
  , "L4Q5"
  , "L4P5"
  ]

-- | Convert STM32xyz to just @Family@
nameToFamily :: String -> Family
nameToFamily n =
  Data.Maybe.fromMaybe
    (error $ "Cannot read family: " ++ n)
    $ readMaybe . fix . drop 5
    $ n
  where
    fix "L4+" = "L4Plus"
    fix x     = x
