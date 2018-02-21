{-# LANGUAGE RecordWildCards #-}

module Data.SVD.Pretty where

import Data.SVD.Types
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf

ppList pp x = vcat $ map pp x

ppDevice res = displayS (renderPretty 0.4 80 (ppDevice' res)) ""
ppPeripheral res = displayS (renderPretty 0.4 80 (ppPeriph res)) ""
ppRegister res = displayS (renderPretty 0.4 80 (ppReg res)) ""

ppDevice' Device{..} =
  (red $ string deviceName)
  <$$> indent 2 (ppList ppPeriph devicePeripherals)

ppPeriph Peripheral{..} =
  hardline
  <> (yellow $ string periphName)
  <+> (white $ ppHex periphBaseAddress)
  <+> (magenta $ string periphDescription)
  <$$> indent 2 (ppList ppReg periphRegisters)
  <//> (maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom)

ppReg Register{..} =
  hardline
  <> (blue $ string regName)
  <+> (white $ ppHex regAddressOffset)
  <+> (cyan $ char '-' <+> (string regDescription))
  <$$> indent 2 (ppList ppField (procFields regFields))

ppHex = text . printf "0x%x"
rpad m xs = take m $ xs ++ repeat ' '

ppField Field{..} =
  (green $ string $ rpad 10 fieldName)
  <+> ppWidthPad 7 fieldBitWidth
  <+> parens (string fieldDescription)

ppWidth 1 = string "Bit"
ppWidth x = string "Bits" <+> int x

ppWidthPad m 1 = string $ rpad m "Bit"
ppWidthPad m x = string $ rpad m $ "Bits " ++ show x

