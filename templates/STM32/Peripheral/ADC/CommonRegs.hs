{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Ivory.BSP.STM32.Peripheral.ADC.CommonRegs where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.ADC.RegTypes

-- ADC Analog-to-digital converter common registers
-- shared between all ADCs

-- ADC Common status register
--  | offset : 0x0
[ivory|
  bitdata ADC_CSR :: Bits 32 = adc_csr
  { _ :: Bits 10
  , adc_csr_ovr3   :: Bit
  , adc_csr_strt3  :: Bit
  , adc_csr_jstrt3 :: Bit
  , adc_csr_jeoc3  :: Bit
  , adc_csr_eoc3   :: Bit
  , adc_csr_awd3   :: Bit
  , _              :: Bits 2
  , adc_csr_ovr2   :: Bit
  , adc_csr_strt2  :: Bit
  , adc_csr_jstrt2 :: Bit
  , adc_csr_jeoc2  :: Bit
  , adc_csr_eoc2   :: Bit
  , adc_csr_awd2   :: Bit
  , _              :: Bits 2
  , adc_csr_ovr1   :: Bit
  , adc_csr_strt1  :: Bit
  , adc_csr_jstrt1 :: Bit
  , adc_csr_jeoc1  :: Bit
  , adc_csr_eoc1   :: Bit
  , adc_csr_awd1   :: Bit
  }
|]

-- ADC Common status register
--  | offset : 0x4
[ivory|
  bitdata ADC_CCR :: Bits 32 = adc_ccr
  { _ :: Bits 8
  , adc_ccr_tsvrefe :: Bit
  , adc_ccr_vbate   :: Bit
  , _               :: Bits 4
  , adc_ccr_adcpre  :: ADCPrescaler
  , adc_ccr_dma     :: ADCDMA
  , adc_ccr_dds     :: Bit
  , _               :: Bit
  , adc_ccr_delay   :: Bits 4
  , _               :: Bits 3
  , adc_ccr_multi   :: Bits 5
  }
|]

-- ADC common regular data register for dual and triple mode
--  | offset : 0x8
[ivory|
  bitdata ADC_CDR :: Bits 32 = adc_cdr
  { adc_dr_data2 :: Bits 16
  , adc_dr_data1 :: Bits 16
  }
|]
