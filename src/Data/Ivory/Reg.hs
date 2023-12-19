{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Ivory.Reg where

import Data.SVD.Types
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Data.Bits.Pretty
import qualified Data.Char

ppIvoryReg
  :: Peripheral
  -> Register
  -> Doc
ppIvoryReg Peripheral{..} Register{..} =
  hardline
  <> (red $ string $ "-- " ++ periphName ++ periphDescription)
  <> hardline
  <> (red $ string "[ivory|\n")
  <+> (string "bitdata ")
  <> (blue $ string $ upcaseRegName)
  <+> (string $ ":: Bits " ++ (show regSize) ++ " = ")
  <> (blue $ string lowcaseRegName)
  <$$> indent 2 ((vcat $ zipWith (<+>) pattern (ppField <$> fmap (prefixRegField $ lowcaseRegName ++ "_") regFields))
                  <$$> (string "}"))
  <$$> (red $ string "|]")
  <$$> mkPeriph
    where
      pattern = (string "{"):(repeat $ string ",")
      prefixRegField :: String -> Field -> Field
      prefixRegField prefix f = f {fieldName = (prefix ++fieldName f)}
      upcaseRegName = mconcat [periphName, "_", regName]
      lowcaseRegName = Data.Char.toLower <$> upcaseRegName
      mkPeriph =
        hardline
        <> (string $ "reg" ++ upcaseRegName ++ " :: BitDataReg " ++ upcaseRegName)
        <> hardline
        <> (string $ mconcat ["reg",
                            upcaseRegName,
                            " = mkBitDataRegNamed (",
                            (Data.Char.toLower <$> periphName),
                            "_periph_base + ", Data.Bits.Pretty.formatHex regAddressOffset, ")",
                            " \"", lowcaseRegName, "\""
                            ])

ppField :: Field -> Doc
ppField Field{..} =
  (green $ string $ rpad 25 (Data.Char.toLower <$> fieldName))
  <+> string "::"
  <+> ppWidthPad 7 fieldBitWidth
  <+> cyan (string $ " -- " ++ fieldDescription)
  where
    ppWidthPad
      :: Int
      -> Int
      -> Doc
    ppWidthPad m 1 = string $ rpad m "Bit"
    ppWidthPad m x = string $ rpad m $ "Bits " ++ show x

    rpad :: Int -> String -> String
    rpad m xs = take m $ xs ++ repeat ' '
