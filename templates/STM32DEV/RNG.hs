module {{ modns }} (
{{#instances}}
{{#prefixRest}}{{/prefixRest}}{{ name }}
{{/instances}}
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.Peripheral.RNG

{{#instances}}
{{ name }} :: RNG
{{ name }} = mkRNG {{ name }}_periph_base
                rccenable rccdisable
               {{#interrupts}} {{ dev }}.{{.}}{{/interrupts}}
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
