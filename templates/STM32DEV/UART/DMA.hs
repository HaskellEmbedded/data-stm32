module {{ modns }} (
{{#prefixedInstances}}
  {{prefix}} dma{{ data.name }}
{{/prefixedInstances}}
  ) where

import Ivory.BSP.STM32.Peripheral.DMA.Peripheral
import Ivory.BSP.STM32.Peripheral.UART.DMA

import Ivory.BSP.STM32{{ dev }}.DMA
import Ivory.BSP.STM32{{ dev }}.{{ periph }}

{{#instances}}
dma{{ name }} :: DMAUART
dma{{ name }} = DMAUART
  { dmaUARTPeriph    = {{ name }}
  , dmaUARTDMAPeriph = dma{{ dmaIndex}}
  , dmaUARTTxStream  = DMAStream {{ dmaTxStream }}
  , dmaUARTTxChannel = DMAChannel {{ dmaTxChannel }}
  , dmaUARTRxStream  = DMAStream {{ dmaRxStream }}
  , dmaUARTRxChannel = DMAChannel {{ dmaRxChannel }}
  }
{{/instances}}
