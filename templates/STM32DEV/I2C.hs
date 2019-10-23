module {{ modns }} (
{{#instances}}
{{#prefixRest}}{{/prefixRest}}{{ name }}
{{/instances}}
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32{{ dev }}.AF
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.AF
import Ivory.BSP.STM32.Peripheral.I2C

{{#instances}}
{{ name }} :: I2C
{{ name }} = mkI2CVersion V{{ version }} {{ name }}_periph_base
                rccenable rccdisable rccreset
               {{#interrupts}} {{ dev }}.{{.}}{{/interrupts}}
                {{ clockSource }}
                (\pins -> findAFByPins pins "{{ name }}" afDB)
                "{{ name }}"

  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccreset   = modifyReg rcc_reg_{{ rccResetReg }}  $ clearBit rcc_{{ rccResetReg }}_{{ rccResetBit }}

{{/instances}}
