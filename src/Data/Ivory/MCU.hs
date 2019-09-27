{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.MCU where

import Data.Ivory.Pretty

import Data.Char (toUpper)
import Data.List.Split (splitOn)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- devices.data
genMCUData f = do
  x <- fmap (map words . filter (\x -> x /= "" && head x /= '#') . lines) $ readFile f
  return $ ppMCUs x

ppMCUs res = displayS (renderPretty 0.4 60 (ppMCUs' res)) ""
ppMCUs' x =
     string "devices ="
  </> indent 2 (encloseStack "[" "]" "," $ map ppMCU x)

ppMCU (pat:attrs) =
     char '('
  <> char '"'
  <> string (fixPat pat)
  <> char '"'
  <> ", defMCU "
  <> encloseStack "{" "}" "," (map printAttr attrs)
  <> char ')'
  where
    printAttr x = case splitOn "=" x of
      (what:cnt:_) -> string $ "mcu" ++ what ++ " = " ++ (addJust what $ ("kbytes "++) $ init cnt)
      _ -> ""
    addJust "CCM" = ("Just $ "++)
    addJust "EEP" = ("Just $ "++)
    addJust "RAM2" = ("Just $ "++)
    addJust _ = id

    fixPat = (replace "*" ".*") . (replace "?" ".") . map toUpper
