module {{ modns }} (
{{#prefixedInstances}}
  {{prefix}} {{ data.name }}
{{/prefixedInstances}}
  ) where


import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32{{ dev }}.AF
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.AF
import Ivory.BSP.STM32.Peripheral.SPI

{{#instances}}
{{ name }} :: SPI
{{ name }} = mkSPI {{ name }}_periph_base
          rccenable rccdisable
         {{#interrupts}} {{ dev }}.{{.}}{{/interrupts}}
          {{ clockSource }}
          (\pin -> findAFByPin pin "{{ name }}" afDB)
          {{ version }}
          "{{ name }}"
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
