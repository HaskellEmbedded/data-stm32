{-# LANGUAGE RecordWildCards #-}
module {{ modns }}
  ( linker_script
  ) where

import Ivory.Artifact
import Ivory.Artifact.Template
import qualified Paths_ivory_bsp_stm32 as P

import Ivory.BSP.STM32.MCU
import Data.STM32

linker_script :: FilePath -> NamedMCU -> Integer -> String -> Artifact
linker_script fname namedMcu bl_offset reset_handler =
  artifactCabalFileTemplate' P.getDataDir path fname (attrs namedMcu)
  where
  path = "support/linker_script.lds.template"
  attrs nmcu@(_name, mcu) =
    [ ( "regions",       memregs nmcu)
    , ( "estack",        show $ (ramOffset nmcu) + sramSize nmcu)
    , ( "reset_handler", reset_handler)
    -- use ccsram if available
    , ( "ccsramOrSram",  maybe "sram" (pure "ccsram") (mcuCcmRam mcu))
    , ( "additionalSections", additionalSections nmcu)
    ]

  -- if continuous use mcuRam if not just sram1
  continuous nmcu@(_name, mcu) =
    (not $ mcuForceSplit mcu) && (ramContinuos $ rams nmcu)

  sramSize nmcu@(_name, MCU{..}) =
    if continuous nmcu then mcuRam
                       else mcuRam1

  memregs nmcu@(_name, MCU{..}) = unlines $ map mkRegion
    ([("flash",  Just $ flashOffset + bl_offset
              ,  Just $ mcuFlash - fromIntegral bl_offset)

    , ("sram",   Just $ fromIntegral $ ramOffset nmcu
             ,   Just $ sramSize nmcu)

    , ("ccsram", fromIntegral <$> ccmOffset nmcu
               , mcuCcmRam)
    , ("eeprom", fromIntegral <$> eepromOffset nmcu
               , mcuEEProm)
    , ("backup", fromIntegral <$> backupOffset nmcu
               , mcuBackupRam)
    ] ++ (extraRams nmcu (continuous nmcu)))

  extraRams :: NamedMCU -> Bool -> [(String, Maybe Integer, Maybe Int)]
  extraRams _                     True = []
  extraRams nmcu@(_name, MCU{..}) False =
    [
      ("sram2", fromIntegral <$> ram2Offset nmcu
              , mcuRam2)
    , ("sram3", fromIntegral <$> ram3Offset nmcu
              , mcuRam3)
    ]

  additionalSections nmcu = concatMap mkSection $ extraRams nmcu (continuous nmcu)
    where mkSection (name, (Just _), (Just _)) = unlines
            -- e.g. .sram2Section : { ... } > sram2
            [ "." ++ sname ++ " : {"
            , ". = ALIGN(4);"
            , "__" ++ sname ++ "_start__ = .;"
            , " *(." ++ sname ++ "*)"
            , "__" ++ sname ++ "_end = .;"
            , "} > " ++ name
            ]
            where
              sname = name ++ "Section"
          mkSection _ = ""

  mkRegion (name, (Just offset), (Just reglength)) =
    "  " ++ name ++ "(" ++ (mode name) ++ "): ORIGIN = " ++ (show offset) ++ ", LENGTH =" ++ (show reglength)
  mkRegion _ = ""

  mode "flash"  = "rx"
  mode "eep"    = "rx"
  mode _        = "rwx"
