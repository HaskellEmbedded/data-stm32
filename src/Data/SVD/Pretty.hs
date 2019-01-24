{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SVD.Pretty where

import Data.SVD.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf
import Data.Char (toLower)
import Data.List (intersperse)

ppList pp x = vcat $ map pp x

ppDevice res = displayS (renderPretty 0.4 80 (ppDevice' res)) ""
ppDeviceInfo res = displayS (renderPretty 0.4 80 (ppDeviceInfo' res)) ""
ppPeripheral res = displayS (renderPretty 0.4 80 (ppPeriph res)) ""
ppRegister res = displayS (renderPretty 0.4 80 (ppReg res)) ""

ppDevice' Device{..} =
  (red $ string deviceName)
  <$$> indent 2 (ppList ppPeriph devicePeripherals)

ppPeriph periph@Peripheral{..} =
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

ppIvoryReg Peripheral{..} Register{..} =
  hardline
  <> (red $ string $ "-- " ++ periphName ++ periphDescription)
  <> hardline
  <> (red $ string "[ivory|\n")
  <+> (string "bitdata ")
  <> (blue $ string $ upcaseRegName)
  <+> (string $ ":: Bits 32 = ")
  <> (blue $ string lowcaseRegName)
  <$$> indent 2 ((vcat $ zipWith (<+>) pattern (ppField <$> procFields (fmap (prefixRegField $ lowcaseRegName ++ "_") regFields)))
                  <$$> (string "}"))
  <$$> (red $ string "|]")
  <$$> mkPeriph
    where
      pattern = (string "{"):(repeat $ string ",")
      prefixRegField :: String -> Field -> Field
      prefixRegField prefix f = f {fieldName = (prefix ++fieldName f)}
      upcaseRegName = mconcat [periphName, "_", regName]
      lowcaseRegName = toLower <$> upcaseRegName
      mkPeriph =
        hardline
        <> (string $ "reg" ++ upcaseRegName ++ " :: BitDataReg " ++ upcaseRegName)
        <> hardline
        <> (string $ mconcat ["reg",
                            upcaseRegName,
                            " = mkBitDataRegNamed (",
                            (toLower <$> periphName),
                            "_periph_base + ", hexFormat regAddressOffset, ")",
                            " \"", lowcaseRegName, "\""
                            ])

hexFormat = printf "0x%x"
ppHex = text . hexFormat
rpad m xs = take m $ xs ++ repeat ' '

ppField Field{..} =
  (green $ string $ rpad 25 (toLower <$> fieldName))
  <+> string "::"
  <+> ppWidthPad 7 fieldBitWidth
  <+> cyan (string $ " -- " ++ fieldDescription)

ppWidth 1 = string "Bit"
ppWidth x = string "Bits" <+> int x

ppWidthPad m 1 = string $ rpad m "Bit"
ppWidthPad m x = string $ rpad m $ "Bits " ++ show x


-- ISR

ppDevISR res = displayS (renderPretty 0.4 80 (ppDevISR' res)) ""

ppDevISR' Device{..} = (ppList ppPeriphISR devicePeripherals)

ppPeriphISR periph@Peripheral{..} =
  indent 2 (ppList ppISR periphInterrupts)
--  <//> (maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom)
--
--


ppISRs res = displayS (renderPretty 0.4 80 (ppList ppISR res)) ""

ppISR Interrupt{..} = indent 2 (
  "|"
  <+> string interruptName
  <> " -- " <> int interruptValue <+> string interruptDescription
  )

-- terse output

ppDeviceInfo' Device{..} =
  (red $ string deviceName)
  <$$> indent 2 (ppList ppPeriphName devicePeripherals)

ppPeriphName periph@Peripheral{..} = (yellow $ string periphName)

-- MemMap

ppMemMap res = displayS (renderPretty 0.4 80 (ppList ppMem res)) ""
ppMem (addr, periph) =
     name <> " :: Integer"
  </> name
  <> " = "
  <> string addr
  where name = string (map toLower periph) <> "_periph_base"
