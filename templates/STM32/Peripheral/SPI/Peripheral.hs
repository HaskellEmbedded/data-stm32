{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module {{ modns }} where

import Control.Monad (when)
import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.SPI.Pins
import Ivory.BSP.STM32.Peripheral.SPI.Regs
import Ivory.BSP.STM32.Peripheral.SPI.RegTypes

import Ivory.BSP.STM32.Peripheral.GPIO

data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , spiRCCEnable   :: forall eff . Ivory eff ()
  , spiRCCDisable  :: forall eff . Ivory eff ()
  , spiInterrupt   :: HasSTM32Interrupt
  , spiPClk        :: PClk
  , spiAFLookup    :: GPIOPin -> GPIO_AF
  , spiVersion     :: Int
  , spiName        :: String
  }

mk{{ type }}
  :: (STM32Interrupt i)
  => Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> i
  -> PClk
  -> (GPIOPin -> GPIO_AF)
  -> Int
  -> String
  -> {{ type }}
mk{{ type }} base rccen rccdis inter pclk afLookup version n = {{ type }}
{{{ bitDataRegsMk }}}
  , spiRCCEnable   = rccen
  , spiRCCDisable  = rccdis
  , spiInterrupt   = HasSTM32Interrupt inter
  , spiPClk        = pclk
  , spiAFLookup    = afLookup
  , spiVersion     = version
  , spiName        = n
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

initInPin :: {{ type }} -> GPIOPin -> Ivory eff ()
initInPin periph pin = do
  comment ("init spi input pin " ++ pinName pin)
  pinEnable  pin
  pinSetAF   pin (spiAFLookup periph pin)
  pinSetMode pin gpio_mode_af
  pinSetPUPD pin gpio_pupd_none
  pinSetSpeed pin gpio_speed_50mhz

initOutPin :: {{ type }} -> GPIOPin -> Ivory eff ()
initOutPin periph pin = do
  comment ("init spi output pin " ++ pinName pin)
  pinEnable        pin
  pinSetAF         pin (spiAFLookup periph pin)
  pinSetMode       pin gpio_mode_af
  pinSetOutputType pin gpio_outputtype_pushpull
  pinSetSpeed      pin gpio_speed_50mhz

-- | Enable peripheral and setup GPIOs. Must be performed
--   before any other SPI peripheral actions.
spiInit :: (GetAlloc eff ~ 'Scope s) => {{ type }} -> SPIPins -> Ivory eff ()
spiInit spi pins = do
  spiRCCEnable spi
  spiClearCr1 spi
  spiClearCr2 spi
  initInPin  spi (spiPinMiso pins)
  initOutPin spi (spiPinMosi pins)
  initOutPin spi (spiPinSck  pins)

spiInitISR :: (GetAlloc eff ~ 'Scope s)
           => {{ type }} -> Ivory eff ()
spiInitISR spi = interrupt_enable $ spiInterrupt spi

-- Clock Polarity and Phase: see description
-- of CPOL and CPHA in ST reference manual RM0090
data SPICSActive      = ActiveHigh | ActiveLow
data SPIClockPolarity = ClockPolarityLow | ClockPolarityHigh
data SPIClockPhase    = ClockPhase1 | ClockPhase2
data SPIBitOrder      = LSBFirst | MSBFirst

data SPIDevice = SPIDevice
  { spiDevPeripheral    :: {{ type }}
  , spiDevCSPin         :: GPIOPin
  , spiDevClockHz       :: Integer
  , spiDevCSActive      :: SPICSActive
  , spiDevClockPolarity :: SPIClockPolarity
  , spiDevClockPhase    :: SPIClockPhase
  , spiDevBitOrder      :: SPIBitOrder
  , spiDevName          :: String
  }

spiDeviceInit :: (GetAlloc eff ~ 'Scope s) => SPIDevice -> Ivory eff ()
spiDeviceInit dev = do
  let pin = spiDevCSPin dev
  pinEnable         pin
  spiDeviceDeselect dev
  pinSetMode        pin gpio_mode_output
  pinSetOutputType  pin gpio_outputtype_pushpull
  pinSetSpeed       pin gpio_speed_2mhz

spiBusBegin :: (GetAlloc eff ~ 'Scope cs)
            => ClockConfig -> SPIDevice -> Ivory eff ()
spiBusBegin clockconfig dev = do
  modifyReg (spiRegCR1 periph) $ clearBit spi_cr1_spe

  let baud = spiDevBaud clockconfig periph (spiDevClockHz dev)
  modifyReg (spiRegCR1 periph) $ do
    setBit   spi_cr1_mstr
    setBit   spi_cr1_ssm
    setBit   spi_cr1_ssi
    setField spi_cr1_br baud
    case spiDevClockPolarity dev of
      ClockPolarityLow  -> clearBit spi_cr1_cpol
      ClockPolarityHigh -> setBit  spi_cr1_cpol
    case spiDevClockPhase dev of
      ClockPhase1 -> clearBit spi_cr1_cpha
      ClockPhase2 -> setBit   spi_cr1_cpha
    case spiDevBitOrder dev of
      LSBFirst -> setBit   spi_cr1_lsbfirst
      MSBFirst -> clearBit spi_cr1_lsbfirst


  -- FIFO reception threshold to 8 bits (default is 16)
  when (spiVersion periph > 2) $ do
    modifyReg (spiRegCR2 periph) $ setBit   spi_cr2_frxth

  -- Enable SPI
  modifyReg (spiRegCR1 periph) $ setBit   spi_cr1_spe
  where
  periph = spiDevPeripheral dev


spiBusEnd :: {{ type }} -> Ivory eff ()
spiBusEnd  periph =
  spiModifyCr1 periph [ spi_cr1_spe ] false

spiDeviceEnd :: SPIDevice -> Ivory eff ()
spiDeviceEnd dev = do
  spiDeviceDeselect dev
  spiBusEnd         periph
  where periph = spiDevPeripheral dev


spiSetTXEIE :: {{ type }} -> Ivory eff ()
spiSetTXEIE spi = modifyReg (spiRegCR2 spi) $ setBit spi_cr2_txeie

spiClearTXEIE :: {{ type }} -> Ivory eff ()
spiClearTXEIE spi = modifyReg (spiRegCR2 spi) $ clearBit spi_cr2_txeie

spiSetRXNEIE :: {{ type }} -> Ivory eff ()
spiSetRXNEIE spi = modifyReg (spiRegCR2 spi) $ setBit spi_cr2_rxneie

spiClearRXNEIE :: {{ type }} -> Ivory eff ()
spiClearRXNEIE spi = modifyReg (spiRegCR2 spi) $ clearBit spi_cr2_rxneie

spiGetDR :: {{ type }} -> Ivory eff Uint8
spiGetDR spi = do
  r <- getReg (spiRegDR spi)
  return (toRep (r #. spi_dr_dr))

spiGetDR16 :: {{ type }} -> Ivory eff Uint16
spiGetDR16 spi = do
  r <- getReg (spiRegDR16 spi)
  return (toRep (r #. spi_dr16_dr))

spiSetDR :: {{ type }} -> Uint8 -> Ivory eff ()
spiSetDR spi b =
  setReg (spiRegDR spi) $
    setField spi_dr_dr (fromRep b)

spiSetDR16 :: {{ type }} -> Uint16 -> Ivory eff ()
spiSetDR16 spi b =
  setReg (spiRegDR16 spi) $
    setField spi_dr16_dr (fromRep b)

-- Internal Helper Functions ---------------------------------------------------

spiDevBaud :: ClockConfig -> {{ type }} -> Integer -> SPIBaud
spiDevBaud clockconfig periph target = foldl aux spi_baud_div_256 tbl
  where
  fpclk = clockPClkHz (spiPClk periph) clockconfig
  aux dflt (divider, spibaud) = if fpclk `div` divider <= target then spibaud else dflt
  tbl = [( 256, spi_baud_div_256 )
        ,( 128, spi_baud_div_128 )
        ,( 64,  spi_baud_div_64 )
        ,( 32,  spi_baud_div_32 )
        ,( 16,  spi_baud_div_16 )
        ,( 8,   spi_baud_div_8 )
        ,( 4,   spi_baud_div_4 )
        ,( 2,   spi_baud_div_2 )]

spiDeviceSelect   :: SPIDevice -> Ivory eff ()
spiDeviceSelect dev = case spiDevCSActive dev of
  ActiveHigh -> pinSet   (spiDevCSPin dev)
  ActiveLow  -> pinClear (spiDevCSPin dev)

spiDeviceDeselect :: SPIDevice -> Ivory eff ()
spiDeviceDeselect dev = case spiDevCSActive dev of
  ActiveHigh -> pinClear (spiDevCSPin dev)
  ActiveLow  -> pinSet   (spiDevCSPin dev)

spiSetBaud :: {{ type }} -> SPIBaud -> Ivory eff ()
spiSetBaud periph baud = modifyReg (spiRegCR1 periph) $ setField spi_cr1_br baud

spiModifyCr1 :: {{ type }} -> [BitDataField SPI_CR1 Bit] -> IBool -> Ivory eff ()
spiModifyCr1 periph fields b =
  modifyReg (spiRegCR1 periph) $ mapM_ (\f -> setField f (boolToBit b)) fields

spiClearCr1 :: {{ type }} -> Ivory eff ()
spiClearCr1 periph = modifyReg (spiRegCR1 periph) $ do
  -- It may not be strictly necessary to clear all of these fields.
  -- I'm copying the implementation of the HWF4 lib, where REG->CR1 is set to 0
  clearBit spi_cr1_bidimode
  clearBit spi_cr1_bidioe
  clearBit spi_cr1_crcen
  clearBit spi_cr1_crcnext
  clearBit spi_cr1_crcl
  clearBit spi_cr1_rxonly
  clearBit spi_cr1_ssm
  clearBit spi_cr1_ssi
  clearBit spi_cr1_lsbfirst
  clearBit spi_cr1_spe
  setField spi_cr1_br spi_baud_div_2
  clearBit spi_cr1_mstr
  clearBit spi_cr1_cpol
  clearBit spi_cr1_cpha

spiClearCr2 :: {{ type }} -> Ivory eff ()
spiClearCr2 periph = modifyReg (spiRegCR2 periph) $ do
  -- May not be strictly necessary to set all these fields, see comment
  -- for spiClearCr1
  clearBit spi_cr2_txeie
  clearBit spi_cr2_rxneie
  clearBit spi_cr2_errie
  clearBit spi_cr2_ssoe
  clearBit spi_cr2_txdmaen
  clearBit spi_cr2_rxdmaen

spiSetClockPolarity :: {{ type }} -> SPIClockPolarity -> Ivory eff ()
spiSetClockPolarity periph polarity =
  modifyReg (spiRegCR1 periph) $ case polarity of
    ClockPolarityLow  -> clearBit spi_cr1_cpol
    ClockPolarityHigh -> setBit  spi_cr1_cpol

spiSetClockPhase :: {{ type }} -> SPIClockPhase -> Ivory eff ()
spiSetClockPhase periph phase =
  modifyReg (spiRegCR1 periph) $ case phase of
    ClockPhase1 -> clearBit spi_cr1_cpha
    ClockPhase2 -> setBit   spi_cr1_cpha

spiSetBitOrder :: {{ type }} -> SPIBitOrder -> Ivory eff ()
spiSetBitOrder periph bitorder =
  modifyReg (spiRegCR1 periph) $ case bitorder of
    LSBFirst -> setBit   spi_cr1_lsbfirst
    MSBFirst -> clearBit spi_cr1_lsbfirst
