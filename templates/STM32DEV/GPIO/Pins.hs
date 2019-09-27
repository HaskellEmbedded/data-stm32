{-# LANGUAGE TemplateHaskell #-}

module {{ modns }} where

import Ivory.BSP.STM32{{ dev }}.GPIO.Ports
import Ivory.BSP.STM32.Peripheral.GPIOv{{ vers }}.Regs
import Ivory.BSP.STM32.Peripheral.GPIOv{{ vers }}.TH

{{#instances}}
mkGPIOPins 'gpio{{ index }} "pin{{ index }}"
{{/instances}}
