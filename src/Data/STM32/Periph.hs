
module Data.STM32.Periph where

data Periph =
    ADC
  | AFIO -- F1 alternate function controls / remapping, proly good idea to turn it on
  | ATIM
  | GTIM
  | CAN
  | CEC
  | CRC
  | CRYP
  | DAC
  | DBG
  | DCMI
  | DMA
  | DMA2D
  | Ethernet
  | EXTI
  | FLASH
  | FPU
  | FSMC
  | GPIO
  | HASH
  | I2C
  | IWDG
  | LPTIM
  | IRTIM
  -- LTDC
  | MPU -- memory protection unit
  | NVIC
  -- PF
  | PWR
  | QUADSPI
  | RCC
  | RNG
  | RTC
  -- SAI -- serial audio interface
  -- SCB
  -- SDMMC
  -- SPDIF_RX
  | SPI
  -- STK
  | SYSCFG
  | TSC -- touchsensing controller
  | UART
  | USART
  | USB_OTG_FS
  | USB_OTG_HS
  | WWDG
  deriving (Show, Eq, Ord)

supportedPeriphs =
 [ CAN
 , I2C
 , GPIO
 , RNG
 , SPI
 , UART ]


