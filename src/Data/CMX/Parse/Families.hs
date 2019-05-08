{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Data.CMX.Parse.Families where

import Control.Monad

import Control.Arrow.ArrowList
import Text.XML.HXT.Core
import qualified Data.Set as Set
import Data.Maybe

import Data.CMX.Types
import Data.CMX.Parse.Util

att  = getAttrValue
text = getChildren >>> getText
textAtTag tag = text <<< atTag tag

-- mcu/families.xml parser
families = atTag "Families" >>>
  proc x -> do
    fams' <- listA family -< x
    returnA -< fams'

family = atTag "Family" >>>
  proc x -> do
    name <- att "Name" -< x
    subFams' <- listA subFamily -< x
    returnA -< (name, subFams')

subFamily = atTag "SubFamily" >>>
  proc x -> do
    name <- att "Name" -< x
    mcus' <- listA mcu -< x
    returnA -< (name, mcus')

mcu = atTag "Mcu" >>>
  proc x -> do
    name <- att "Name" -< x
    refName <- att "RefName" -< x
    rpn <- att "RPN" -< x
    -- rest of the tags are redundant as they appear
    -- in mcu/<name>.xml (handled by mcu parser)

    periphs <- listA periph -< x

    returnA -< (name, refName, rpn, periphs)

periph = atTag "Peripheral" >>>
  proc x -> do
    typ <- att "Type" -< x
    maxOccurs <- att "MaxOccurs" -< x
    returnA -< (typ, maxOccurs)


parseFamilies f = do
  res <- runX (readDocument [] f >>> families)
  case res of
    [] -> return $ Left "no families parsed"
    xs -> return $ Right xs
