{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.STM32.Name where

import GHC.Generics (Generic)
import Control.Applicative
import Data.Serialize
import Data.ByteString.Char8 (ByteString)

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8
import qualified Data.Maybe

import Data.STM32.Family

-- | Data type to hole parsed full ST device name
-- like "STM32F103C8T6"
-- > STM32DevName F1 "03" (Just 'C') (Just '8') (Just 'T') (Just '6')
data STM32DevName = STM32DevName {
    stmFam         :: Family      -- ^ STM32 device family i.e. F1
  , stmName        :: String      -- ^ Device name e.g. 03 for STM32F103
  , stmPinCountId  :: Maybe Char
  , stmFlashSizeId :: Maybe Char
  , stmPackage     :: Maybe Char
  , stmTempRange   :: Maybe Char
  } deriving (Eq, Ord, Show, Generic)

instance Serialize STM32DevName

-- | Parse a string with device name into @STM32DevName@
parseName :: ByteString -> Either String STM32DevName
parseName = parseOnly devNameParser

-- | Parse a string like STM32F427
-- into `STM32DevName` and try to find the best
-- match from list of `STM32DevName`s.
--
-- Allows more specific strings like @STM32F427RG@
-- or complete one like @STM32F103C8T6@.
findBest :: String -> [STM32DevName] -> [STM32DevName]
findBest x = filter (\d ->
     stmFam px == stmFam d
  && stmName px == stmName d
  && maybeMatches px d
  )
  where px = case parseName $ Data.ByteString.Char8.pack x of
                Left e -> error $ "Unable to parse device name '" ++ x ++ "' error was " ++ e
                Right y -> y
        maybeMatches y d = all (testMatch y d) [stmPinCountId, stmFlashSizeId, stmPackage]
        testMatch y d field | Data.Maybe.isJust $ field y = field y == field d
        testMatch _ _ _ = True

-- | Check if `STM32DevName` matches supplied `String`
matches :: String -> STM32DevName -> Bool
matches x devName = not $ null $ findBest x [devName]

-- | Parser for @STM32DevName@
devNameParser :: Parser STM32DevName
devNameParser = do
  _ <- string "STM32"
  stmFam' <- familyParser
  stmName <- (\a b -> [a, b]) <$> anyChar <*> anyChar
  stmPinCountId <- optional anyChar
  stmFlashSizeId <- optional anyChar
  stmPackage <- optional anyChar
  stmTempRange <- optional $ notChar 'x'

  stmFam <- case stmName of
    "R5" -> return L4Plus
    "S5" -> return L4Plus
    "GB" -> return G4
    _    -> return stmFam'

  return $ STM32DevName{..}

-- | Turn @STM32DevName@ to a full device name
-- > showName $ STM32DevName F1 "03" (Just 'C') (Just '8') (Just 'T') Nothing
-- "STM32F103C8Tx"
showName :: STM32DevName -> String
showName STM32DevName{..} = concat [
    "STM32"
  , showFam stmFam
  , stmName
  ] ++ map (Data.Maybe.fromMaybe 'x') [
    stmPinCountId
  , stmFlashSizeId
  , stmPackage
  , stmTempRange
  ]

-- | Turn @STM32DevName@ to a short device name e.g. "F103" or "F427"
shortName :: STM32DevName -> String
shortName STM32DevName{..} = showFam stmFam ++ stmName


