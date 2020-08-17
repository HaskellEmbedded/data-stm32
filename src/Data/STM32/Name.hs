{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.STM32.Name where

import GHC.Generics (Generic)
import Control.Applicative
import Data.Serialize

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Maybe

import Data.STM32.Family

data STM32DevName = STM32DevName {
    stmFam         :: Family
  , stmName        :: String
  , stmPinCountId  :: Maybe Char
  , stmFlashSizeId :: Maybe Char
  , stmPackage     :: Maybe Char
  , stmTempRange   :: Maybe Char
  } deriving (Eq, Ord, Show, Generic)

instance Serialize STM32DevName


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
  where px = case parseName $ B.pack x of
                Left e -> error $ "Unable to parse device name '" ++ x ++ "' error was " ++ e
                Right x -> x
        maybeMatches x d = and $ map (testMatch x d) [stmPinCountId, stmFlashSizeId, stmPackage]
        testMatch x d field | isJust $ field x = field x == field d
        testMatch x d field | otherwise = True


-- | Check if `STM32DevName` matches supplied `String`
matches :: String -> STM32DevName -> Bool
matches x devName = not $ null $ findBest x [devName]

devNameParser :: Parser STM32DevName
devNameParser = do
  string "STM32"
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

-- > showName $ STM32DevName F1 "03" (Just 'C') (Just '8') (Just 'T') Nothing
-- "STM32F103C8Tx"
showName STM32DevName{..} = concat [
    "STM32"
  , showFam stmFam
  , stmName
  ] ++ map (maybe 'x' id) [
    stmPinCountId
  , stmFlashSizeId
  , stmPackage
  , stmTempRange
  ]

-- "F103"
shortName STM32DevName{..} = showFam stmFam ++ stmName


