module Ivory.BSP.STM32.Peripheral.EXTI.Types where

import Ivory.BSP.STM32.Peripheral.GPIO (GPIOPin, GPIOPull)

data EXTIEdge = Rising | Falling | Both
  deriving (Show, Eq, Ord)

data EXTIPin = EXTIPin {
    extiPin  :: GPIOPin
  , extiEdge :: EXTIEdge
  , extiPull :: GPIOPull
  } deriving (Show)
