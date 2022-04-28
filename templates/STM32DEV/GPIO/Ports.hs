{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module {{ modns }} (
{{#prefixedInstances}}
  {{prefix}} gpio{{ data.index }}
{{/prefixedInstances}}
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap

import Ivory.BSP.STM32.Peripheral.GPIOv{{ vers }}.Peripheral

{{#instances}}
gpio{{ index }} :: GPIOPort
gpio{{ index }} = mkGPIOPort {{ name }}_periph_base rccenable rccdisable {{ numericIndex }}
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
