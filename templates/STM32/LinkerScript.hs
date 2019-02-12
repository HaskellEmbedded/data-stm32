{-# LANGUAGE RecordWildCards #-}
module @modns@
  ( linker_script
  ) where

import Ivory.Artifact
import Ivory.Artifact.Template
import qualified Paths_ivory_bsp_stm32 as P
import Ivory.BSP.STM32.Core
import Ivory.BSP.STM32.Family
import Ivory.BSP.STM32.MCU


linker_script :: FilePath -> MCU -> Integer -> String -> Artifact
linker_script fname mcu bl_offset reset_handler =
  artifactCabalFileTemplate' P.getDataDir path fname (attrs mcu)
  where
  path = "support/linker_script.lds.template"
  attrs MCU{..} =
    [ ( "regions",       memregs mcu)
    , ( "estack",        show $ (ramOffset mcuFamily) + mcuRAM)
    , ( "reset_handler", reset_handler)
    -- use ccsram if available
    , ( "ccsramOrSram",  maybe "sram" (pure "ccsram") mcuCCM)
    ]

  memregs MCU{..} = unlines $ map mkRegion
    [ ("flash",  flashOffset + bl_offset, Just $ mcuROM - bl_offset)
    , ("sram",   ramOffset mcuFamily,     Just mcuRAM)
    , ("ccsram", ccmOffset mcuFamily,     mcuCCM)
   -- XXX: handle these as well
   -- , ("sram2",   ramOffset?? mcuFamily, mcuRAM2)
   -- , ("eep",   eepOffset?? mcuFamily, mcuEEP)
    ]

  mkRegion (_, _,         Nothing)          = ""
  mkRegion (name, offset, (Just reglength)) =
    "  " ++ name ++ "(" ++ (mode name) ++ "): ORIGIN = " ++ (show offset) ++ ", LENGTH =" ++ (show reglength)

  mode "flash"  = "rx"
  mode "eeprom" = "rx"
  mode _        = "rwx"

{-
    flash (rx) : ORIGIN = $flash_origin$, LENGTH = $flash_length$
    sram (rwx) : ORIGIN = $sram_origin$, LENGTH = $sram_length$
    ccsram (rwx) : ORIGIN = $ccsram_origin$, LENGTH = $ccsram_length$
-}
