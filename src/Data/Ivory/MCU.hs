{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.MCU where

import Data.Ivory.Pretty

import Data.Char (toUpper)
import Data.List.Split (splitOn)

import Prettyprinter

-- devices.data
genMCUData f = do
  x <- fmap (map words . filter (\x -> x /= "" && head x /= '#') . lines) $ readFile f
  return $ ppMCUs x

ppMCUs res = displayIvory

ppMCUs' x =
     "devices ="
  <> line
  <> indent 2 (encloseStack "[" "]" "," $ map ppMCU x)

ppMCU (pat:attrs) =
     pretty '('
  <> pretty '"'
  <> pretty (fixPat pat)
  <> pretty '"'
  <> ", defMCU "
  <> encloseStack "{" "}" "," (map printAttr attrs)
  <> pretty ')'
  where
    printAttr x = case splitOn "=" x of
      (what:cnt:_) -> pretty $ "mcu" ++ what ++ " = " ++ (addJust what $ ("kbytes "++) $ init cnt)
      _ -> ""
    addJust "CCM" = ("Just $ "++)
    addJust "EEP" = ("Just $ "++)
    addJust "RAM2" = ("Just $ "++)
    addJust _ = id

    fixPat = (replace "*" ".*") . (replace "?" ".") . map toUpper
