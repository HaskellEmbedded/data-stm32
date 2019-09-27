{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module {{ modns }}
  ( init_clocks
  ) where

import Ivory.Language

import Ivory.BSP.STM32.ClockConfig
import Data.STM32

{{#families}}
import qualified Ivory.BSP.STM32{{.}}.ClockInit as {{.}}
{{/families}}

init_clocks :: Family -> ClockConfig -> Def('[]':->())
{{#families}}
init_clocks {{.}} = {{.}}.init_clocks
{{/families}}
init_clocks f = error $ "Family not supported" ++ show f
