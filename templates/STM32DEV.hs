module {{ modns }} (
{{#imImports}}
{{#prefixRest}}{{/prefixRest}}module Ivory.BSP.STM32{{ imDev }}.{{ . }}
{{/imImports}}
  ) where

{{#imImports}}
import Ivory.BSP.STM32{{ imDev }}.{{ . }}
{{/imImports}}
