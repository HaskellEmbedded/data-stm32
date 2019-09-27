module {{ modns }} (
{{#instances}}
{{#prefixRest}}{{/prefixRest}}{{ name }}
{{/instances}}
  , canFilters
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

canFilters :: CANPeriphFilters
canFilters = mkCANPeriphFilters {{ instances.0.name }}_periph_base
                rccenable rccdisable
  where
  rccenable  = modifyReg rcc_reg_{{ instances.0.rccEnableReg }} $ setBit   rcc_{{ instances.0.rccEnableReg }}_{{ instances.0.rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ instances.0.rccEnableReg }} $ clearBit rcc_{{ instances.0.rccEnableReg }}_{{ instances.0.rccEnableBit }}

{{#instances}}
{{ name }} :: CANPeriph
{{ name }} = mkCANPeriph {{ name }}_periph_base
                rccenable rccdisable
               {{#interrupts}} {{ dev }}.{{.}}{{/interrupts}}
               "{{ name }}"
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
