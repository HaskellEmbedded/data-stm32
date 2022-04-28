module {{ modns }} (
{{#prefixedInstances}}
  {{prefix}} {{ data.name }}
{{/prefixedInstances}}
  , canFilters
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

canFilters :: CANPeriphFilters
canFilters = mkCANPeriphFilters {{ firstInstance.name }}_periph_base
                rccenable rccdisable
  where
  rccenable  = modifyReg rcc_reg_{{ firstInstance.rccEnableReg }} $ setBit   rcc_{{ firstInstance.rccEnableReg }}_{{ firstInstance.rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ firstInstance.rccEnableReg }} $ clearBit rcc_{{ firstInstance.rccEnableReg }}_{{ firstInstance.rccEnableBit }}

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
