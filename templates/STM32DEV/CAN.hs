module {{ modns }} (
{{#prefixedInstances}}
  {{prefix}} {{ data.name }}
{{/prefixedInstances}}
{{#canFilterInstances}}
  , {{ canFiltersName }}
{{/canFilterInstances}}
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32{{ dev }}.AF
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.AF
import Ivory.BSP.STM32.Peripheral.CAN

{{#instances}}
{{ name }} :: CANPeriph
{{ name }} = mkCANPeriph {{ name }}_periph_base
                rccenable rccdisable
               {{#interrupts}} {{ dev }}.{{.}}{{/interrupts}}
               (\pin -> findAFByPin pin "{{ name }}" afDB)
               "{{ name }}"
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}

{{#canFilterInstances}}
{{ canFiltersName }} :: CANPeriphFilters
{{ canFiltersName }} =
  mkCANPeriphFilters
    {{ name }}_periph_base
    rccenable
    rccdisable
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/canFilterInstances}}
