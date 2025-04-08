{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ivory.Periph where

import Data.SVD.Types
import Data.Char (toLower, toUpper, isDigit)
import Data.List (intersperse, isPrefixOf)

import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), color)

import Data.Ivory.Pretty
import qualified Data.Bits.Pretty

ppPeriphRegs res = displayIvoryCompact (ppPeriphRegs' res False)
ppPeriphRegsWithDefs res = displayIvoryCompact (ppPeriphRegs' res True)

ppBitDataRegs res = displayIvoryCompact (ppBitDataRegs' res)
ppBitDataRegsMk res = displayIvoryCompact (ppBitDataRegsMk' res)

ppPeriphRegs' periph@Peripheral{..} withDefs =
     comment (pretty periphName <+> pretty periphDescription)
  <> hardline
  <> comment ("Base address:" <+> pretty (Data.Bits.Pretty.formatHex periphBaseAddress))
  <> vsep (map (\reg -> ppIvoryReg periph reg withDefs) periphRegisters)
  <> maybe mempty (\x -> softline <> pretty ("Derived from" :: String) <+> pretty x) periphDerivedFrom

ppIvoryReg Peripheral{..} r@Register{..} withDefs =
      hardline
  <>  comment (pretty regDescription)
  <>  comment (" | offset :" <+> pretty (Data.Bits.Pretty.formatHex regAddressOffset))
  <>  comment (" | address:" <+> pretty (Data.Bits.Pretty.formatHex (periphBaseAddress + regAddressOffset)))
  <>  (annotate (color Red) "[ivory|\n")
  <+> "bitdata "
  <>  (annotate (color Blue) $ pretty upcaseRegName)
  <+> (pretty $ ":: Bits " ++ (show regSize) ++ " = ")
  <>  (annotate (color Blue) $ pretty lowcaseRegName)
  <>  line
  <>  vsep
   [ indent 2 ( encloseStack "{" "}" "," (ppField maxFieldNameLength maxTypeNameLength <$> prefixed))
   , (annotate (color Red) "|]")
   ]
  <>  if withDefs then line <> defs else mempty
    where
      prefixed = fmap (prefixRegField $ lowcaseRegName ++ "_") regFields
      prefixRegField :: String -> Field -> Field
      prefixRegField prefix f | not $ fieldReserved f = f {fieldName = (prefix ++fieldName f)}
      prefixRegField prefix f = f
      upcaseRegName = toUpper <$> mconcat [periphName, "_", regName]
      lowcaseRegName = toLower <$> upcaseRegName
      maxFieldNameLength = maximum . map (length . fieldName) $ prefixed
      maxTypeNameLength = maximum . map ((maybe maxBitsLength length) . fieldRegType) $ prefixed
        where
          maxBitsLength =  maximum . map (length . renderWidth . fieldBitWidth) $ prefixed
          renderWidth 1 = "Bit"
          renderWidth x = "Bits " ++ show x

      defs = defName
           <> " :: BitDataReg "
           <> pretty (toUpper <$> mconcat [periphName, "_", regName])
           <> line
           <> defName <> " = mkBitDataRegNamed"
           <+> parens (pretty (mconcat [toLower <$> periphName, "_periph_base + ", Data.Bits.Pretty.formatHex regAddressOffset]))
           <+> dquotes (pretty (toLower <$> regName))
      defName = pretty (toLower <$> mconcat [ periphName, "_reg_", regName] )


ppBitDataRegs' Peripheral{..} = indent 2 $ encloseSep "{" "" "," $
     map (\Register{..} ->
                   space
                <> pretty (rpad maxRegLength ((toLower <$> periphName) ++ (regOrPort periphName) ++ regName))
                <> " :: BitDataReg "
                <> pretty (toUpper <$> mconcat [periphName, "_", regName])
       ) periphRegisters
  where
    regOrPort x | "GPIO" `isPrefixOf` x = "Port"
    regOrPort _ = "Reg"
    maxRegLength = maximum . ((length (periphName ++ "RCCDisable")):) . map ((+3) . (+length periphName). length . regName) $ periphRegisters

-- register definitions with constructors
ppBitDataRegsMk' Peripheral{..} = indent 2 $ encloseSep "{" "" "," $
      map (\Register{..} ->
                   space
                <> pretty (rpad maxRegLength ((toLower <$> periphName) ++  (regOrPort periphName) ++ regName))
                <> " = reg"
                <+> pretty (Data.Bits.Pretty.formatHex regAddressOffset)
                <+> dquotes (pretty (toLower <$> regName))
       ) periphRegisters
  where
    regOrPort x | "GPIO" `isPrefixOf` x = "Port"
    regOrPort _ = "Reg"
    maxRegLength = maximum . ((length (periphName ++ "RCCDisable")):) . map ((+3) . (+length periphName). length . regName) $ periphRegisters

ppField maxNameLen maxTypeLen f@Field{..} =
  (annotate (color Green) $ pretty $ rpad maxNameLen (toLower <$> fieldName))
  <+> "::"
  <+> (maybe (ppWidthPad maxTypeLen fieldBitWidth) (pretty . rpad maxTypeLen) fieldRegType)
  <+> (annotate (color Cyan) (pretty $ "-- " ++ fieldDescription))

ppWidthPad m 1 = pretty $ rpad m "Bit"
ppWidthPad m x = pretty $ rpad m $ "Bits " ++ show x
