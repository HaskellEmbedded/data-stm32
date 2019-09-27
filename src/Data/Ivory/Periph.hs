{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ivory.Periph where

import Data.SVD.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Data.Char (toLower, toUpper, isDigit)
import Data.List (intersperse, isPrefixOf)

import Data.Ivory.Pretty

ppList pp x = vcat $ map pp x

ppPeriphRegs res = displayIvoryCompact (ppPeriphRegs' res False)
ppPeriphRegsWithDefs res = displayIvoryCompact (ppPeriphRegs' res True)

ppBitDataRegs res = displayIvoryCompact (ppBitDataRegs' res)
ppBitDataRegsMk res = displayIvoryCompact (ppBitDataRegsMk' res)

ppPeriphRegs' periph@Peripheral{..} withDefs =
       comment (string periphName <+> string periphDescription)
  <>   hardline
  <>   comment ("Base address:" <+> ppHex periphBaseAddress)
  <>   ppList (\reg -> ppIvoryReg periph reg withDefs) periphRegisters
  <//> maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom

ppIvoryReg Peripheral{..} r@Register{..} withDefs =
       hardline
  <>   comment (string regDescription)
  <>   comment (string " | offset :" <+> string (hexFormat regAddressOffset))
  <>   comment (string " | address:" <+> string (hexFormat (periphBaseAddress + regAddressOffset)))
  <>   (red $ string "[ivory|\n")
  <+>  (string "bitdata ")
  <>   (blue $ string upcaseRegName)
  <+>  (string $ ":: Bits " ++ (show regSize) ++ " = ")
  <>   (blue $ string lowcaseRegName)
  <$$> indent 2 ( encloseStack "{" "}" "," (ppField maxFieldLength <$> prefixed))
  <$$> (red $ string "|]")
  <$$> (if withDefs then defs else empty)
    where
      prefixed = fmap (prefixRegField $ lowcaseRegName ++ "_") regFields
      prefixRegField :: String -> Field -> Field
      prefixRegField prefix f | not $ fieldReserved f = f {fieldName = (prefix ++fieldName f)}
      prefixRegField prefix f = f
      upcaseRegName = toUpper <$> mconcat [periphName, "_", regName]
      lowcaseRegName = toLower <$> upcaseRegName
      maxFieldLength = maximum . map (length . fieldName) $ prefixed
      defs = defName
           <> string " :: BitDataReg "
           <> string (toUpper <$> mconcat [periphName, "_", regName])
           <$$> defName <> string " = mkBitDataRegNamed"
           <+> parens (string (mconcat [toLower <$> periphName, "_periph_base + ", hexFormat regAddressOffset]))
           <+> dquotes (string (toLower <$> regName))
      defName = string (toLower <$> mconcat [ periphName, "_reg_", regName] )


ppBitDataRegs' Peripheral{..} = indent 2 $ encloseSep "{" "" "," $
     map (\Register{..} ->
                   space
                <> string (rpad maxRegLength ((toLower <$> periphName) ++ (regOrPort periphName) ++ regName))
                <> string " :: BitDataReg "
                <> string (toUpper <$> mconcat [periphName, "_", regName])
       ) periphRegisters
  where
    regOrPort x | "GPIO" `isPrefixOf` x = "Port"
    regOrPort _ = "Reg"
    maxRegLength = maximum . ((length (periphName ++ "RCCDisable")):) . map ((+3) . (+length periphName). length . regName) $ periphRegisters

-- register definitions with constructors
ppBitDataRegsMk' Peripheral{..} = indent 2 $ encloseSep "{" "" "," $
      map (\Register{..} ->
                   space
                <> string (rpad maxRegLength ((toLower <$> periphName) ++  (regOrPort periphName) ++ regName))
                <> string " = reg"
                <+> string (hexFormat regAddressOffset)
                <+> dquotes (string (toLower <$> regName))
       ) periphRegisters
  where
    regOrPort x | "GPIO" `isPrefixOf` x = "Port"
    regOrPort _ = "Reg"
    maxRegLength = maximum . ((length (periphName ++ "RCCDisable")):) . map ((+3) . (+length periphName). length . regName) $ periphRegisters

ppField maxLen f@Field{..} =
  (green $ string $ rpad maxLen (toLower <$> fieldName))
  <> fixPadding fieldReserved
  <> string "::"
  <+> (maybe (ppWidthPad 7 fieldBitWidth) string fieldRegType)
  <+> cyan (string $ " -- " ++ fieldDescription)  -- ++ show f) -- debug show
  where fixPadding True = mempty
        fixPadding False  = string "  "

ppWidth 1 = string "Bit"
ppWidth x = string "Bits" <+> int x

ppWidthPad m 1 = string $ rpad m "Bit"
ppWidthPad m x = string $ rpad m $ "Bits " ++ show x
