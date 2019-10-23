module {{ modns }} (
{{#instances}}
{{#prefixRest}}{{/prefixRest}}{{ name }}
{{/instances}}
  ) where

import Ivory.BSP.STM32{{ dev }}.Clock (clockLSI)
import Ivory.BSP.STM32{{ dev }}.MemoryMap

import Ivory.BSP.STM32.Peripheral.IWDG

{{#instances}}
{{ name }} :: IWDG
{{ name }} = mkIWDG {{ name }}_periph_base clockLSI

{{/instances}}
