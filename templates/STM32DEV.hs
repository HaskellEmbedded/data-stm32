module {{ modns }} (
{{#imImports}}
  {{prefix}} module Ivory.BSP.STM32{{ imDev }}.{{ data }}
{{/imImports}}
  ) where

{{#imImports}}
import Ivory.BSP.STM32{{ imDev }}.{{ data }}
{{/imImports}}
