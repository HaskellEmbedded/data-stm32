
module {{ modns }}
  ( Core(..)
  , core
  , cpu
  , fpu
  , floatabi
  , freertosCore
  ) where

--import Ivory.Tower.Config
import qualified Data.List as L

import Ivory.BSP.STM32.Family
import Ivory.BSP.STM32.MCU

data Core = CortexM0 | CortexM0Plus | CortexM3 | CortexM4F | CortexM7F
  deriving (Eq, Show)

core :: Family -> Core
core F0 = CortexM0
core F1 = CortexM3
core F2 = CortexM3
core F3 = CortexM4F
core F4 = CortexM4F
core F7 = CortexM7F
core H7 = CortexM7F
core L0 = CortexM0Plus
core L1 = CortexM3
core L4 = CortexM4F
core L4Plus = CortexM4F
core W = CortexM3
core T = CortexM3

freertosCore :: MCU -> String
-- only these two match CM7/r0p1 freertos port
freertosCore mcu | "STM32F74" `L.isPrefixOf` (mcuName mcu) = "CM7F"
freertosCore mcu | "STM32F75" `L.isPrefixOf` (mcuName mcu) = "CM7F"
freertosCore mcu | otherwise = coreStr $ core $ mcuFamily mcu
  where
   coreStr :: Core -> String
   coreStr CortexM0     = "CM0"
   coreStr CortexM0Plus = "CM0"
   coreStr CortexM3     = "CM3"
   coreStr CortexM4F    = "CM4F"
   coreStr CortexM7F    = "CM4F"

-- -mcpu for GCC/LD
cpu :: Core -> String
cpu CortexM0     = "cortex-m0"
cpu CortexM0Plus = "cortex-m0plus"
cpu CortexM3     = "cortex-m3"
cpu CortexM4F    = "cortex-m4"
cpu CortexM7F    = "cortex-m7"

-- -mfpu for GCC
fpu :: Core -> String
fpu CortexM0 = "soft"
fpu CortexM0Plus = "soft"
fpu CortexM3 = "soft"
fpu CortexM4F = "fpv4-sp-d16"
fpu CortexM7F = "fpv5-sp-d16"

-- -mfloat-abi for GCC
floatabi :: Core -> Maybe String
floatabi CortexM4F = Just "hard"
floatabi CortexM7F = Just "hard"
floatabi _ = Nothing
