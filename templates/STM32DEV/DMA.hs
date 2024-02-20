module {{ modns }}
  ( dma1
  , dma2
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Interrupt (HasSTM32Interrupt(..))
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.Peripheral.DMA

ahb1Enable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
ahb1Enable bit = modifyReg rcc_reg_ahb1enr (setBit bit)

ahb1Disable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
ahb1Disable bit = modifyReg rcc_reg_ahb1enr (clearBit bit)

dma1 :: DMA
dma1 = mkDMA dma1_periph_base
             (ahb1Enable  rcc_ahb1enr_dma1en)
             (ahb1Disable rcc_ahb1enr_dma1en)
             ints
             "dma1"
  where
  ints = DMAInterrupt
    { dmaInterruptStream0 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM0
    , dmaInterruptStream1 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM1
    , dmaInterruptStream2 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM2
    , dmaInterruptStream3 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM3
    , dmaInterruptStream4 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM4
    , dmaInterruptStream5 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM5
    , dmaInterruptStream6 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM6
    , dmaInterruptStream7 = HasSTM32Interrupt {{ dev }}.DMA1_STREAM7
    }

dma2 :: DMA
dma2 = mkDMA dma2_periph_base
             (ahb1Enable  rcc_ahb1enr_dma2en)
             (ahb1Disable rcc_ahb1enr_dma2en)
             ints
             "dma2"
  where
  ints = DMAInterrupt
    { dmaInterruptStream0 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM0
    , dmaInterruptStream1 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM1
    , dmaInterruptStream2 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM2
    , dmaInterruptStream3 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM3
    , dmaInterruptStream4 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM4
    , dmaInterruptStream5 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM5
    , dmaInterruptStream6 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM6
    , dmaInterruptStream7 = HasSTM32Interrupt {{ dev }}.DMA2_STREAM7
    }
