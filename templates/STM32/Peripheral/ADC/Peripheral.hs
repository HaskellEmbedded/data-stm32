{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Interrupt
import {{ init_modns }}.Regs
import {{ init_modns }}.RegTypes

data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , adcRCCEnable :: forall eff . Ivory eff ()
  , adcRCCDisable :: forall eff . Ivory eff ()
  , adcInt :: HasSTM32Interrupt
  , adcName :: String
  }

-- | Create ADC given the base register address, global interrupt and its name
mk{{ type }}
       :: (STM32Interrupt i)
       => Integer -- ^ Base
       -> (forall eff . Ivory eff ()) -- ^ RCC Enable
       -> (forall eff . Ivory eff ()) -- ^ RCC Disable
       -> i -- ^ global adc interrupt. NB: shared with other adc periphs!
       -> String -- ^ Name
       -> {{ type }}
mk{{ type }} base rccen rccdis int n = {{ type }}
{{{ bitDataRegsMk }}}
    , adcRCCEnable  = rccen
    , adcRCCDisable = rccdis
    , adcInt        = HasSTM32Interrupt int
    , adcName       = n
    }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

adcInit :: ADC
        -> ADCResolution -- ^ how many bits of precision to use in conversion?
        -> IBool -- ^ left-align converted bits in 16-bit data register?
        -> Ivory eff ()
adcInit periph res align = do
  adcRCCEnable periph
  modifyReg (adcRegCR1 periph) $ do
    setField adc_cr1_res res
  modifyReg (adcRegCR2 periph) $ do
    setField adc_cr2_align $ boolToBit align
    setBit adc_cr2_adon

adcStartConversion :: ADC -> Int -> Ivory eff ()
adcStartConversion periph chan = do
  setReg (adcRegSQR3 periph) $ do
    setField adc_sqr3_sq1 $ fromRep $ fromIntegral chan
  setReg (adcRegSQR1 periph) $ do
    setField adc_sqr1_l $ fromRep 1
  modifyReg (adcRegCR2 periph) $ do
    setBit adc_cr2_swstart
    clearBit adc_cr2_eocs
    clearBit adc_cr2_dma
    clearBit adc_cr2_cont

adcGetConversion :: ADC -> Ivory eff Uint16
adcGetConversion periph = do
  dr <- getReg (adcRegDR periph)
  return (toRep (dr #. adc_dr_data))

