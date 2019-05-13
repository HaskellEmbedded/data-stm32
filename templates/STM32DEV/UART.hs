module {{ modns }} where



import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ fam }}.Interrupt as {{ dev }}

{-
import Ivory.BSP.STM32.Peripheral.UART{{version}}.Peripheral
import Ivory.BSP.STM32.Peripheral.UART{{version}}.Regs
import Ivory.BSP.STM32.Peripheral.UART{{version}}.Types
-}
import Ivory.BSP.STM32.Peripheral.UART


-- uart1, uart2, uart3, uart4, uart5, uart6 :: UART
uart1 :: UART
uart1 = mkUARTVersion V{{ version }} usart1_periph_base
                rccenable rccdisable
                {{ dev }}.USART1 PClk2 "uart1" -- XXX PClk1 or 2 or X? similar to APB_X_
  where
  rccenable  = modifyReg rcc_reg_apb2enr $ setBit   rcc_apb2enr_usart1en
  rccdisable = modifyReg rcc_reg_apb2enr $ clearBit rcc_apb2enr_usart1en
