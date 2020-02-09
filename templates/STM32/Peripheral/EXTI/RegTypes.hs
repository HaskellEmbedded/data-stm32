{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Peripheral.EXTI.RegTypes where

import Ivory.Language
import Ivory.HW
import Ivory.HW.BitData
import Ivory.HW.Reg

-- common EXTICR definition
-- - for most variants these are part of SYSCFG register
-- - for G0s its part of EXTI itself
[ivory|
  bitdata EXTI_EXTICR :: Bits 32 = exti_exticr
   { exti_exticr_data  :: Bits 32 }
|]

cvtReg :: BitDataReg a -> BitDataReg EXTI_EXTICR
cvtReg bdr = mkBitDataRegNamed (regAddr bdr) (maybe "" id (bdr_name bdr))

regAddr :: BitDataReg a -> Integer
regAddr r = case bdr_reg r of Reg a -> a


