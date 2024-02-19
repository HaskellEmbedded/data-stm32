{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
--
-- GPIO.hs --- GPIO Peripheral driver.
-- Defines peripheral types, instances, and public API.
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Peripheral.{{ type }}{{ version }}.Regs
import Ivory.BSP.STM32.Peripheral.{{ type }}{{ version }}.RegTypes

-- | A GPIO port, defined as the set of registers that operate on all
-- the pins for that port.
data {{ type }}Port = {{ type }}Port
{{ bitDataRegs }}
  , gpioPortRCCEnable   :: forall eff . Ivory eff ()
  , gpioPortRCCDisable  :: forall eff . Ivory eff ()
  , gpioPortNumber      :: Int
  , gpioPortName        :: String
  }

-- | Create a GPIO port given the base register address.
mk{{ type }}Port
  :: Integer
  -> (forall eff . Ivory eff ())
  -> (forall eff . Ivory eff ())
  -> Int
  -> {{ type }}Port
mk{{ type }}Port base rccen rccdis idx = {{ type }}Port
{{{ bitDataRegsMk }}}
  , gpioPortRCCEnable      = rccen
  , gpioPortRCCDisable     = rccdis
  , gpioPortNumber         = idx
  , gpioPortName           = n
  }
  where
  n = "gpio" ++ [toEnum (fromEnum 'A' + idx)]
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

-- | A GPIO alternate function register and bit field.
data GPIOPinCRMode = CRModeLow  (BitDataField GPIO_CRL GPIOF1_Mode)
                   | CRModeHigh (BitDataField GPIO_CRH GPIOF1_Mode)

data GPIOPinCRConf = CRConfLow  (BitDataField GPIO_CRL (Bits 2))
                   | CRConfHigh (BitDataField GPIO_CRH (Bits 2))

-- | A GPIO pin, defined as the accessor functions to manipulate the
-- bits in the registers for the port the pin belongs to.
data GPIOPin = GPIOPin
  { gpioPinPort         :: GPIOPort
  , gpioPinNumber       :: Int
  , gpioPinMode_F       :: GPIOPinCRMode
  , gpioPinConf_F       :: GPIOPinCRConf
  , gpioPinODR_F        :: BitDataField GPIO_ODR Bit
  , gpioPinIDR_F        :: BitDataField GPIO_IDR Bit
  , gpioPinSetBSRR_F    :: BitDataField GPIO_BSRR Bit
  , gpioPinClearBSRR_F  :: BitDataField GPIO_BSRR Bit
  , gpioPinSetLCKR_F    :: BitDataField GPIO_LCKR Bit
  }

pinName :: GPIOPin -> String
pinName p = gpioPortName (gpioPinPort p) ++ show (gpioPinNumber p)

-- | Enable the GPIO port for a pin in the RCC.
pinEnable :: GPIOPin -> Ivory eff ()
pinEnable = gpioPortRCCEnable . gpioPinPort

-- XXX: this looks bogus, we don't want to turn off WHOLE gpio port
pinDisable :: GPIOPin -> Ivory eff ()
pinDisable = gpioPortRCCDisable . gpioPinPort

-- | Set a GPIO to a default floating input state
pinUnconfigure :: GPIOPin -> Ivory eff ()
pinUnconfigure p = do
  pinDisable p
  pinSetMode p gpio_mode_input
  pinSetInputConfig p gpio_input_conf_float

setRegF :: (BitData a, BitData b, IvoryIOReg (BitDataRep a),
            SafeCast (BitDataRep b) (BitDataRep a))
        => (GPIOPort -> BitDataReg a)
        -> (GPIOPin  -> BitDataField a b)
        -> GPIOPin
        -> b
        -> Ivory eff ()
setRegF reg field pin val =
  modifyReg (reg $ gpioPinPort pin) $
    setField (field pin) val

pinSetMode :: GPIOPin -> GPIOF1_Mode -> Ivory eff ()
pinSetMode pin mode =
  case gpioPinMode_F pin of
    CRModeLow  field -> setRegF gpioPortCRL (const field) pin mode
    CRModeHigh field -> setRegF gpioPortCRH (const field) pin mode

pinSetConf :: GPIOPin -> (Bits 2) -> Ivory eff ()
pinSetConf pin conf =
  case gpioPinConf_F pin of
    CRConfLow  field -> setRegF gpioPortCRL (const field) pin conf
    CRConfHigh field -> setRegF gpioPortCRH (const field) pin conf

pinSetInputConfig :: GPIOPin -> GPIOF1_InputConfig -> Ivory eff ()
pinSetInputConfig pin conf = pinSetConf pin (fromRep . toRep $ conf)

pinSetOutputConfig :: GPIOPin -> GPIOF1_OutputConfig -> Ivory eff ()
pinSetOutputConfig pin conf = pinSetConf pin (fromRep . toRep $ conf)

pinGetMode :: GPIOPin -> Ivory eff GPIOF1_Mode
pinGetMode pin = case gpioPinMode_F pin of
    CRModeLow  field -> do
      r <- getReg (gpioPortCRL $ gpioPinPort pin)
      return $ r #. field
    CRModeHigh field -> do
      r <- getReg (gpioPortCRH $ gpioPinPort pin)
      return $ r #. field

pinGetConf :: GPIOPin -> Ivory eff (Bits 2)
pinGetConf pin = case gpioPinConf_F pin of
    CRConfLow  field -> do
      r <- getReg (gpioPortCRL $ gpioPinPort pin)
      return $ r #. field
    CRConfHigh field -> do
      r <- getReg (gpioPortCRH $ gpioPinPort pin)
      return $ r #. field

pinGetOutputConfig :: GPIOPin -> Ivory eff GPIOF1_OutputConfig
pinGetOutputConfig pin = do
  x <- pinGetConf pin
  return $ fromRep $ bitsToRep x

pinIsAF :: GPIOPin -> Ivory eff IBool
pinIsAF pin = do
  oc <- toRep <$> pinGetOutputConfig pin
  return (oc ==? toRep gpio_output_conf_af_pushpull .|| oc ==? toRep gpio_output_conf_af_opendrain)

pinIsPushPull :: GPIOPin -> Ivory eff IBool
pinIsPushPull pin = do
  oc <- toRep <$> pinGetOutputConfig pin
  return (oc ==? toRep gpio_output_conf_pushpull .|| oc ==? toRep gpio_output_conf_af_pushpull)

pinSet :: GPIOPin -> Ivory eff ()
pinSet pin =
  modifyReg (gpioPortBSRR $ gpioPinPort pin) $
    setBit (gpioPinSetBSRR_F pin)

pinClear :: GPIOPin -> Ivory eff ()
pinClear pin =
  modifyReg (gpioPortBSRR $ gpioPinPort pin) $
    setBit (gpioPinClearBSRR_F pin)

pinRead :: GPIOPin -> Ivory eff IBool
pinRead pin = do
  r <- getReg (gpioPortIDR $ gpioPinPort pin)
  return (bitToBool (r #. gpioPinIDR_F pin))

