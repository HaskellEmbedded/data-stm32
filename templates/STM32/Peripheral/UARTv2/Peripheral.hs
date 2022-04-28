{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
--
-- Peripheral.hs --- UART Peripheral Description
-- UART Peripheral type and constructor
--

module {{ modns }} where

import Ivory.Language

import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.GPIO

import Ivory.BSP.STM32.Peripheral.UART.Pins
import Ivory.BSP.STM32.Peripheral.UART.RegTypes
import Ivory.BSP.STM32.Peripheral.{{ type }}{{ version }}.Regs

data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , uartRCCEnable  :: forall eff . Ivory eff ()
  , uartRCCDisable :: forall eff . Ivory eff ()
  , uartInterrupt  :: HasSTM32Interrupt
  , uartPClk       :: PClk
  , uartAFLookup   :: GPIOPin -> GPIO_AF
  , uartName       :: String
  }

mk{{ type }} :: (STM32Interrupt i)
       => Integer
       -> (forall eff . Ivory eff ())
       -> (forall eff . Ivory eff ())
       -> i
       -> PClk
       -> (GPIOPin -> GPIO_AF)
       -> String
       -> {{ type }}
mk{{ type }} base rccen rccdis interrupt pclk afLookup n = {{ type }}
{{{ bitDataRegsMk }}}
  , uartRCCEnable  = rccen
  , uartRCCDisable = rccdis
  , uartInterrupt  = HasSTM32Interrupt interrupt
  , uartPClk       = pclk
  , uartAFLookup   = afLookup
  , uartName       = n
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)


-- | Initialize GPIO pins for a UART.
initPin :: {{ type }} -> GPIOPin -> Ivory eff ()
initPin uart p = do
  pinEnable        p
  pinSetSpeed      p gpio_speed_50mhz
  pinSetOutputType p gpio_outputtype_pushpull
  pinSetPUPD       p gpio_pupd_pullup
  pinSetAF         p (uartAFLookup uart p)
  pinSetMode       p gpio_mode_af

-- | Set the BRR register of a UART given a baud rate.
setBaudRate :: (GetAlloc eff ~ 'Scope s)
            => UART -> ClockConfig -> Uint32 -> Ivory eff ()
setBaudRate uart clockconfig baud = do
  --pclk    <- assign =<< getFreqPClk platform (uartPClk uart)
  pclk    <- assign (fromIntegral (clockPClkHz (uartPClk uart)
                                    clockconfig))
  cr1     <- getReg (uartRegCR1 uart)
  isOver8 <- assign (bitToBool (cr1 #. uart_cr1_over8))
  ipart   <- assign ((25 * pclk) `iDiv` (baud * (isOver8 ? (2, 4))))
  mask    <- assign ((ipart `iDiv` 100) `iShiftL` 4)
  fpart   <- assign (ipart - (100 * (mask `iShiftR` 4)))
  brr     <- assign (mask .|
                     (isOver8 ?
                      ( (((fpart * 8)  + 50) `iDiv` 100) .& 0x07
                      , (((fpart * 16) + 50) `iDiv` 100) .& 0x0f)))
  setReg (uartRegBRR uart) $ do
    setField uart_brr_div (fromRep (lbits brr))

-- | Configure the stop bits for a UART.
setStopBits :: UART -> UART_StopBits -> Ivory eff ()
setStopBits uart x =
  modifyReg (uartRegCR2 uart) $
    setField uart_cr2_stop x

-- | Configure the word length for a UART.
setWordLen :: UART -> UART_WordLen -> Ivory eff ()
setWordLen uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_m x

-- | Configure the parity setting for a UART.
setParity :: UART -> IBool -> Ivory eff ()
setParity uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_pce (boolToBit x)

-- | Initialize a UART device given a baud rate.
uartInit :: (GetAlloc eff ~ 'Scope s)
         => UART -> UARTPins -> ClockConfig -> Uint32 -> Bool -> Ivory eff ()
uartInit uart pins clockconfig baud useinterrupts = do
  -- Enable the peripheral clock and set up GPIOs.
  uartRCCEnable uart
  initPin uart (uartPinTx pins)
  initPin uart (uartPinRx pins)

  -- Initialize the baud rate and other settings.
  setBaudRate uart clockconfig baud
  setStopBits uart uart_stop_bits_1_0
  setWordLen  uart uart_word_len_8
  setParity   uart false

  interrupt_enable $ uartInterrupt uart

  -- enable rxne interrupt, transmitter, and receiver.
  modifyReg (uartRegCR1 uart) $ do
    -- XXX: we shouldn't have to set txeie here - but if we don't, txe interrupt
    -- will never fire until an rxneie interrupt has occured. this wasn't the
    -- case in the old hwf4 code, and i can't explain the root cause of this
    -- bug.
    case useinterrupts of
      True -> do
        setBit uart_cr1_txeie
        setBit uart_cr1_rxneie
      _ -> return ()

    setBit uart_cr1_te
    setBit uart_cr1_re

  -- Enable the UART
  modifyReg (uartRegCR1 uart) $ do
    setBit uart_cr1_ue

-- | Set the UART data register.
setDR :: UART -> Uint8 -> Ivory eff ()
setDR uart b =
  setReg (uartRegDR uart) $
    setField uart_dr_dr (fromRep b)

-- | Read the UART data register.
readDR :: UART -> Ivory eff Uint8
readDR uart = do
  dr <- getReg (uartRegDR uart)
  return (toRep (dr #. uart_dr_dr))

-- | Enable or disable the "TXE" interrupt.
setTXEIE :: UART -> IBool -> Ivory eff ()
setTXEIE uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_txeie (boolToBit x)

-- | See whether "TXE" interrupt is enabled.
getTXEIE :: UART -> Ivory eff IBool
getTXEIE uart = do
  cr1 <- getReg (uartRegCR1 uart)
  return $ bitToBool (cr1 #. uart_cr1_txeie)

-- | Enable or disable the "RXNE" interrupt.
setRXNEIE :: UART -> IBool -> Ivory eff ()
setRXNEIE uart x =
  modifyReg (uartRegCR1 uart) $
    setField uart_cr1_rxneie (boolToBit x)
