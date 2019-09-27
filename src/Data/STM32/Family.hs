{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.STM32.Family where

import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Data.Serialize
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.Foldable

data Family =
    F0
  | F1
  | F2
  | F3
  | F4
  | F7
  | H7
  | L0
  | L1
  | L4
  | L4Plus -- L4S L4R
  | L5
  | G0
  | G4
  | WB
  | TS
  | MP1
  deriving (Eq, Ord, Show, Read, Generic)

instance Serialize Family

showFam L4Plus = "L4"
showFam x = show x

familyParser :: Parser Family
familyParser = do
  f <- asum
    [ pure MP1 <$> string "MP1"
    , (\a b -> read $ [a, b]) <$> anyChar <*> anyChar
    ]

  return f

supportedFamilies =
  [ F0
  , F1
  , F3
  , F4
  , F7
  , L4
  , L4Plus
  , G0
  , G4 ]

-- convert STM32xyz to Family
nameToFamily :: String -> Family
nameToFamily n = maybe (error $ "Cannot read family: " ++ n) id $ readMaybe . fix . drop 5 $ n
  where
    fix "L4+" = "L4Plus"
    fix x     = x


