{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Parse.Families (parseFamilies) where

import Control.Monad

import Control.Arrow.ArrowList
import Text.XML.HXT.Core
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

import Data.CMX.Types
import Data.CMX.Parse.Util
import Data.STM32.Types (nameToFamily)

att  = getAttrValue
text = getChildren >>> getText
textAtTag tag = text <<< atTag tag

-- mcu/families.xml parser
families = atTag "Families" >>>
  proc x -> do
    fams' <- listA family -< x
    returnA -< Map.fromList fams'

family = atTag "Family" >>>
  proc x -> do
    name' <- att "Name" -< x
    subFams' <- listA subFamily -< x
    let
      name = nameToFamily name'
    returnA -< (name, subFams')

subFamily = atTag "SubFamily" >>>
  proc x -> do
    name <- att "Name" -< x
    mcus' <- listA mcu -< x
    returnA -< SubFamily name mcus'

mcu = atTag "Mcu" >>>
  proc x -> do
    smcuName <- att "Name" -< x
    smcuRefName <- att "RefName" -< x
    smcuRPN <- att "RPN" -< x
    smcuRam <- (arr ((*1024) . read) <<< textAtTag "Ram") -< x
    smcuFlash <- (arr ((*1024) . read) <<< textAtTag "Flash") -< x
    -- rest of the tags are redundant as they appear
    -- in mcu/<name>.xml (handled by mcu parser)

    smcuPeriphs <- listA periph -< x

    returnA -< ShortMCU {..}

periph = atTag "Peripheral" >>>
  proc x -> do
    typ <- att "Type" -< x
    maxOccurs' <- att "MaxOccurs" -< x
    let
      maxOccurs = read maxOccurs'

    returnA -< ShortPeriph typ maxOccurs

parseFamilies f = do
  res <- runX (readDocument [] f >>> families)
  case res of
    []  -> return $ error "no families parsed"
    [x] -> return x
