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
import Ivory.BSP.STM32.Family

import qualified Paths_ivory_bsp_stm32 as P

{% for fam in fams %}
import qualified Ivory.BSP.STM32{{ fam }}.Interrupt as {{ fam }}
{% endfor %}

byFamily :: Family -> [(String, String)]
{% for fam in fams %}
byFamily {{ fam }} = attrs {{ fam }}.WWDG
{% endfor %}

reset_handler :: String
reset_handler = exceptionHandlerName Reset

vector_table :: Family -> Located Artifact
vector_table family =
  Src $ artifactCabalFileTemplate P.getDataDir fname (byFamily family)
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
