{-# LANGUAGE DataKinds #-}
module {{ modns }} ({{#instances}}{{ name }}{{/instances}}) where

import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.Peripheral.EXTI

{{#instances}}
{{ name }} :: EXTI
{{ name }} = mkEXTIVersion
  V{{ version }}
  {{ name }}_periph_base
  undefined -- syscfg rcc enable not required
  undefined -- syscfg rcc disable not required
  undefined -- extiCRx is set via `fix` in EXTIv2.Peripheral to point to internal regs
  undefined
  undefined
  undefined
  [
{{#extiInterrupts}}
    {{ prefix }} ({{ data.rangeStart }}, {{ data.rangeEnd }}, {{ dev }}.{{ data.rangeISR }})
{{/extiInterrupts}}
  ]
{{/instances}}
