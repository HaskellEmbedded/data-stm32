module {{ modns }}
  ( ATIM(..)
{{#instances}}
  , {{ name }}
{{/instances}}
  , module Ivory.BSP.STM32.Peripheral.ATIM.Regs
  , module Ivory.BSP.STM32.Peripheral.ATIM.RegTypes
  ) where

import Ivory.BSP.STM32.Peripheral.ATIM.Peripheral
import Ivory.BSP.STM32.Peripheral.ATIM.Regs
import Ivory.BSP.STM32.Peripheral.ATIM.RegTypes

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap

{{#instances}}
{{ name }} :: ATIM
{{ name }} =
  mkATIM
    tim{{ index }}_periph_base
    rccenable
    rccdisable
    "{{ name }}"
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
