{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module {{ modns }}
  ( vector_table
  , reset_handler
  ) where

import Ivory.Artifact
import Ivory.Artifact.Template
import Ivory.BSP.ARMv7M.Exception
import Ivory.BSP.STM32.Interrupt
import Data.CMX
import Data.STM32

import qualified Paths_ivory_bsp_stm32 as P

{{#shortDevices}}
import qualified Ivory.BSP.STM32{{.}}.Interrupt as {{.}}
{{/shortDevices}}

byDevice :: (STM32DevName, MCU) -> [(String, String)]
{{#shortDevices}}
byDevice (n, _d) | shortName n == "{{.}}" = attrs {{.}}.WWDG
{{/shortDevices}}
byDevice (n, _d) = error $ "Device not supported" ++ show n

reset_handler :: String
reset_handler = exceptionHandlerName Reset

vector_table :: (STM32DevName, MCU) -> Located Artifact
vector_table x =
  Src $ artifactCabalFileTemplate P.getDataDir fname (byDevice x)
  where
  fname = "support/vector_table.s.template"

attrs :: forall i . (STM32Interrupt i) => i -> [(String, String)]
attrs i = [ ("entries", entries)
          , ("weakdefs", weakdefs)
          , ("reset_handler", reset_handler)
          ]
  where
  itable :: [Maybe i]
  itable = interruptTable i
  entries = unlines $
    map (entry . (fmap exceptionHandlerName)) exceptionTable ++
    map (entry . (fmap interruptHandlerName)) itable
  weakdefs = unlines $
    map (weakdef . (fmap exceptionHandlerName)) (drop 1 exceptionTable) ++
    map (weakdef . (fmap interruptHandlerName)) itable

  entry (Just e) = "\t.word " ++ e
  entry Nothing  = "\t.word 0"
  weakdef (Just handler) = "\t.weak " ++ handler ++
    "\n\t.thumb_set " ++ handler ++ ",defaultExceptionHandler\n"
  weakdef Nothing = ""
