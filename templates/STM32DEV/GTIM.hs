module {{ modns }}
  ( GTIM(..)
  , GTIM32
  , GTIM16
{{#instances}}
  , tim{{ index }}
{{/instances}}
{{#instances32bit}}
  , tim{{ index }}_32
{{/instances32bit}}
  , module Ivory.BSP.STM32.Peripheral.GTIM.Regs
  , module Ivory.BSP.STM32.Peripheral.GTIM.RegTypes
  ) where

import Ivory.BSP.STM32.Peripheral.GTIM.Peripheral
import Ivory.BSP.STM32.Peripheral.GTIM.Regs
import Ivory.BSP.STM32.Peripheral.GTIM.RegTypes

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap

{{#instances}}
tim{{ index }} :: GTIM16
tim{{ index }} =
  mkGTIM
    tim{{ index }}_periph_base
    rccenable
    rccdisable
    "tim{{ index }}"
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
-- Both TIM2 and TIM5 are really 32 bit timers, but you can safely make
-- them believe they are 16 bit.
{{#instances32bit}}
tim{{ index }}_32 :: GTIM32
tim{{ index }}_32 =
  mkGTIM
    tim{{ index }}_periph_base
    rccenable
    rccdisable
    "tim{{ index }}_32bit"
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances32bit}}
