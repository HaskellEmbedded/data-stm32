{-# LANGUAGE DataKinds #-}
module {{ modns }} ({{#instances}}{{ name }}{{/instances}}) where

import Ivory.BSP.STM32{{ dev }}.MemoryMap
import Ivory.BSP.STM32{{ dev }}.AFIO
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.Peripheral.EXTI

{{#instances}}
{{ name }} :: EXTI
{{ name }} = mkEXTIVersion
  V{{ version }}
  {{ name }}_periph_base
  (afioRCCEnable afio)
  (afioRCCDisable afio)
  (cvtReg $ afioRegEXTICR1 afio)
  (cvtReg $ afioRegEXTICR2 afio)
  (cvtReg $ afioRegEXTICR3 afio)
  (cvtReg $ afioRegEXTICR4 afio)
  [
{{#extiInterrupts}}
  {{#prefixRest}}{{/prefixRest}}({{ rangeStart }}, {{ rangeEnd }}, {{ dev }}.{{ rangeISR }})
{{/extiInterrupts}}
  ]
{{/instances}}
