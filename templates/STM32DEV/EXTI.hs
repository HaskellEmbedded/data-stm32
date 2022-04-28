{-# LANGUAGE DataKinds #-}
module {{ modns }} ({{#instances}}{{ name }}{{/instances}}) where

import Ivory.BSP.STM32{{ dev }}.MemoryMap
import Ivory.BSP.STM32{{ dev }}.SYSCFG
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.Peripheral.EXTI

{{#instances}}
{{ name }} :: EXTI
{{ name }} = mkEXTIVersion
  V{{ version }}
  {{ name }}_periph_base
  (syscfgRCCEnable syscfg)
  (syscfgRCCDisable syscfg)
  (cvtReg $ syscfgRegEXTICR1 syscfg)
  (cvtReg $ syscfgRegEXTICR2 syscfg)
  (cvtReg $ syscfgRegEXTICR3 syscfg)
  (cvtReg $ syscfgRegEXTICR4 syscfg)
  [
{{#extiInterrupts}}
    {{ prefix }} ({{ data.rangeStart }}, {{ data.rangeEnd }}, {{ dev }}.{{ data.rangeISR }})
{{/extiInterrupts}}
  ]
{{/instances}}
