{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ivory.Periph where

import Data.SVD.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Printf
import Data.Char (toLower, toUpper, isDigit)
import Data.List (intersperse, isPrefixOf)

import Data.Ivory.Pretty

ppList pp x = vcat $ map pp x

ppPeriphRegs res = displayIvoryCompact (ppPeriphRegs' res)
ppBitDataRegs res = displayIvoryCompact (ppBitDataRegs' res)
ppBitDataRegsMk res = displayIvoryCompact (ppBitDataRegsMk' res)

filterFirst p [] = []
filterFirst p (x:xs)
    | p x       = x:filterFirst p xs
    | otherwise = xs

-- this drops first number from names
-- can6_lal6 -> can_lal6
dropFirstDigit = filterFirst (not . isDigit)

-- from SPI1 we get SPI
-- from GPIOD we get GPIO
name x | "GPIO" `isPrefixOf` x = string . ("GPIO"++) . drop 5 $ x
name x | "gpio" `isPrefixOf` x = string . ("gpio"++) . drop 5 $ x
name x | otherwise = string . dropFirstDigit $ x


ppPeriphRegs' periph@Peripheral{..} =
       comment (name periphName <+> string periphDescription)
  <>   hardline
  <>   comment ("Base address:" <+> ppHex periphBaseAddress)
  <>   ppList (ppIvoryReg periph) periphRegisters
  <//> maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom

ppIvoryReg Peripheral{..} Register{..} =
       hardline
  <>   comment (string regDescription)
  <>   comment (string " | offset :" <+> string (hexFormat regAddressOffset))
  <>   comment (string " | address:" <+> string (hexFormat (periphBaseAddress + regAddressOffset)))
  <>   (red $ string "[ivory|\n")
  <+>  (string "bitdata ")
  <>   (blue $ name upcaseRegName)
  <+>  (string $ ":: Bits 32 = ")
  <>   (blue $ name lowcaseRegName)
  <$$> indent 2 ( encloseStack "{" "}" "," (ppField maxFieldLength <$> procFields prefixed))
  <$$> (red $ string "|]")
    where
      prefixed = fmap (prefixRegField $ lowcaseRegName ++ "_") regFields
      prefixRegField :: String -> Field -> Field
      prefixRegField prefix f = f {fieldName = (prefix ++fieldName f)}
      upcaseRegName = toUpper <$> mconcat [periphName, "_", regName]
      lowcaseRegName = toLower <$> upcaseRegName
      maxFieldLength = maximum . map (length . fieldName) $ prefixed

ppBitDataRegs' Peripheral{..} = indent 2 $ encloseSep "{" "" "," $
     map (\Register{..} ->
                   space
                <> name (rpad maxRegLength ((toLower <$> periphName) ++ (regOrPort periphName) ++ regName))
                <> string " :: BitDataReg "
                <> name (toUpper <$> mconcat [periphName, "_", regName])
       ) periphRegisters
  where
    regOrPort "GPIOA" = "Port"
    regOrPort "GPIOD" = "Port"
    regOrPort _ = "Reg"
    maxRegLength = maximum . ((length (periphName ++ "RCCDisable")):) . map ((+3) . (+length periphName). length . regName) $ periphRegisters

ppBitDataRegsMk' Peripheral{..} = indent 2 $ encloseSep "{" "" "," $
      map (\Register{..} ->
                   space
                <> name (rpad maxRegLength ((toLower <$> periphName) ++  (regOrPort periphName) ++ regName))
                <> string " = reg"
                <+> string (hexFormat regAddressOffset)
                <+> dquotes (string (toLower <$> regName))
       ) periphRegisters
  where
    regOrPort "GPIOA" = "Port"
    regOrPort "GPIOD" = "Port"
    regOrPort _ = "Reg"
    maxRegLength = maximum . ((length (periphName ++ "RCCDisable")):) . map ((+3) . (+length periphName). length . regName) $ periphRegisters

hexFormat = printf "0x%x"
ppHex = text . hexFormat

rpad m xs | m <= length xs = xs
rpad m xs | otherwise = take m $ xs ++ repeat ' '

ppField maxLen Field{..} =
  (green $ name $ rpad maxLen (toLower <$> fieldName))
  <> fixPadding fieldReserved
  <> string "::"
  <+> (maybe (ppWidthPad 7 fieldBitWidth) string fieldRegType)
  <+> cyan (string $ " -- " ++ fieldDescription)
  where fixPadding True = mempty
        fixPadding False  = string "  "

ppWidth 1 = string "Bit"
ppWidth x = string "Bits" <+> int x

ppWidthPad m 1 = string $ rpad m "Bit"
ppWidthPad m x = string $ rpad m $ "Bits " ++ show x

