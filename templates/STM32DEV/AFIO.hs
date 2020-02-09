module {{ modns }}
  ( module {{ modns }}.Peripheral
  , module {{ modns }}.Regs
  , afio
  ) where

import Ivory.Language
import Ivory.HW

import {{ modns }}.Peripheral
import {{ modns }}.Regs
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap (afio_periph_base)

{{#instances}}
{{ name }} :: AFIO
{{ name }} = mkAFIO {{ name }}_periph_base rccenable rccdisable
  where
  rccenable  = modifyReg rcc_reg_{{ rccEnableReg }} $ setBit   rcc_{{ rccEnableReg }}_{{ rccEnableBit }}
  rccdisable = modifyReg rcc_reg_{{ rccEnableReg }} $ clearBit rcc_{{ rccEnableReg }}_{{ rccEnableBit }}

{{/instances}}
