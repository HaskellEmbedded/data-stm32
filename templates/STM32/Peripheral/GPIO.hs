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

module {{ modns }} (
    pinName
  , pinNumber
  , pinPort
  , pinPortNumber
  , pinEnable
  , pinDisable
  , pinUnconfigure
  , pinSetMode
  , pinSetOutputType
  , pinSetSpeed
  , pinSetPUPD
  , pinSetPull
  , pinSetAF
  , pinSet
  , pinClear
  , pinRead
  , GPIOPin(..)
  , GPIOPull(..)
  , pinIsV1
  , pinIsV2
  , module Ivory.BSP.STM32.Peripheral.GPIOv2.RegTypes
  ) where

import Ivory.Language
import Ivory.Stdlib

import qualified Ivory.BSP.STM32.Peripheral.GPIOv1.Peripheral as V1
import qualified Ivory.BSP.STM32.Peripheral.GPIOv1.RegTypes   as V1

import qualified Ivory.BSP.STM32.Peripheral.GPIOv2.Peripheral as V2
import qualified Ivory.BSP.STM32.Peripheral.GPIOv2.RegTypes   as V2

-- due to re-export
import Ivory.BSP.STM32.Peripheral.GPIOv2.RegTypes

-- | Wrapper around GPIO versions
data GPIOPin = GPIOF1 V1.GPIOPin | GPIOFX V2.GPIOPin

-- | Pull up/down resistor configuration
data GPIOPull = PullUp | PullDown | NoPull
  deriving (Show, Eq, Ord)

-- | Return pretty name of the `GPIOPin`
--
-- >>> pinName gpioA7
-- "gpioA7"
pinName :: GPIOPin -> String
pinName (GPIOFX p) = V2.pinName p
pinName (GPIOF1 p) = V1.pinName p

instance Show GPIOPin where
  show = pinName

-- | Return index of the `GPIOPin`
--
-- >>> pinNumber gpioA7
-- 7
pinNumber :: GPIOPin -> Int
pinNumber (GPIOFX p) = V2.gpioPinNumber p
pinNumber (GPIOF1 p) = V1.gpioPinNumber p

-- | Return name of the port for `GPIOPin`
--
-- >>> pinPort gpioA0
-- "gpioA"
pinPort :: GPIOPin -> String
pinPort (GPIOFX p) = V2.gpioPortName $ V2.gpioPinPort p
pinPort (GPIOF1 p) = V1.gpioPortName $ V1.gpioPinPort p

-- | Return port index for `GPIOPin`
--
-- >>> pinPortNumber gpioA0
-- 0
-- >>> pinPortNumber gpioB0
-- 1
pinPortNumber :: GPIOPin -> Int
pinPortNumber (GPIOFX p) = V2.gpioPortNumber $ V2.gpioPinPort p
pinPortNumber (GPIOF1 p) = V1.gpioPortNumber $ V1.gpioPinPort p

-- | Is version 1 `GPIOPin` (F1 like)
pinIsV1 :: GPIOPin -> Bool
pinIsV1 (GPIOF1 _) = True
pinIsV1 _          = False

-- | Is version 2 `GPIOPin` (F4 and others)
pinIsV2 :: GPIOPin -> Bool
pinIsV2 (GPIOFX _) = True
pinIsV2 _          = False

-- | Enable the GPIO port for a pin in the RCC.
pinEnable :: GPIOPin -> Ivory eff ()
pinEnable (GPIOFX p) = V2.pinEnable p
pinEnable (GPIOF1 p) = V1.pinEnable p

-- XXX: disables port?
pinDisable :: GPIOPin -> Ivory eff ()
pinDisable (GPIOFX p) = V2.pinDisable p
pinDisable (GPIOF1 p) = V1.pinDisable p

-- | Set a GPIO to a default floating input state
pinUnconfigure :: GPIOPin -> Ivory eff ()
pinUnconfigure (GPIOFX p) = V2.pinUnconfigure p
pinUnconfigure (GPIOF1 p) = V1.pinUnconfigure p

-- | Set pin to high state
pinSet :: GPIOPin -> Ivory eff ()
pinSet (GPIOFX p) = V2.pinSet p
pinSet (GPIOF1 p) = V1.pinSet p

-- | Set pin to low state
pinClear :: GPIOPin -> Ivory eff ()
pinClear (GPIOFX p) = V2.pinClear p
pinClear (GPIOF1 p) = V1.pinClear p

-- | Read pin value
pinRead :: GPIOPin -> Ivory eff IBool
pinRead (GPIOFX p) = V2.pinRead p
pinRead (GPIOF1 p) = V1.pinRead p

pinSetAF :: GPIOPin -> V2.GPIO_AF -> Ivory eff ()
pinSetAF (GPIOFX pin) af = V2.pinSetAF pin af
pinSetAF (GPIOF1 _pin) _af = return ()
-- XXX: ^ silently ignore AF for F1??

-- | Configure pin mode - input, output, analog or alternate function
pinSetMode :: GPIOPin -> V2.GPIO_Mode -> Ivory eff ()
pinSetMode (GPIOFX pin) v2mode = V2.pinSetMode pin v2mode
pinSetMode (GPIOF1 pin) v2mode = do
  cond_
    [ v2mode ==? V2.gpio_mode_input ==> do

        V1.pinSetMode        pin V1.gpio_mode_input
        V1.pinSetInputConfig pin V1.gpio_input_conf_float

    , v2mode ==? V2.gpio_mode_analog ==> do

        V1.pinSetMode        pin V1.gpio_mode_input
        V1.pinSetInputConfig pin V1.gpio_input_conf_analog

    , v2mode ==? V2.gpio_mode_output ==> do

        V1.pinSetMode pin V1.gpio_mode_output_50mhz

    , v2mode ==? V2.gpio_mode_af ==> do

        --- set AF but keep pushpull/opendrain
        pp <- V1.pinIsPushPull pin
        ifte_ pp
          (V1.pinSetOutputConfig pin V1.gpio_output_conf_af_pushpull)
          (V1.pinSetOutputConfig pin V1.gpio_output_conf_af_opendrain)
    ]

-- | Configure pin speed
pinSetSpeed :: GPIOPin -> V2.GPIO_Speed -> Ivory eff ()
pinSetSpeed (GPIOFX pin) v2speed = V2.pinSetSpeed pin v2speed
pinSetSpeed (GPIOF1 pin) v2speed = do
  -- only 2 and 50mhz are directly compatible
  assert (v2speed /=? V2.gpio_speed_25mhz)
  assert (v2speed /=? V2.gpio_speed_100mhz)
  v1mode <- V1.pinGetMode pin
  -- only if we are not in input mode
  unless (v1mode ==? V1.gpio_mode_input) $ do
    cond_
      [ v2speed ==? V2.gpio_speed_2mhz  ==>
          V1.pinSetMode pin V1.gpio_mode_output_2mhz
      , v2speed ==? V2.gpio_speed_50mhz ==>
          V1.pinSetMode pin V1.gpio_mode_output_50mhz
      ]


-- | Set output type of the `GPIOPin`, either `push-pull` or `open-drain`
pinSetOutputType :: GPIOPin -> V2.GPIO_OutputType -> Ivory eff ()
pinSetOutputType (GPIOFX pin) v2typ = V2.pinSetOutputType pin v2typ
pinSetOutputType (GPIOF1 pin) v2typ = do
  --- set opendrain or pushpull, keep AF
  cond_
    [ v2typ ==? V2.gpio_outputtype_opendrain ==> do
        af <- V1.pinIsAF pin
        ifte_ af
          (V1.pinSetOutputConfig pin V1.gpio_output_conf_af_opendrain)
          (V1.pinSetOutputConfig pin V1.gpio_output_conf_opendrain)
    , v2typ ==? V2.gpio_outputtype_pushpull  ==> do
        af <- V1.pinIsAF pin
        ifte_ af
          (V1.pinSetOutputConfig pin V1.gpio_output_conf_af_pushpull)
          (V1.pinSetOutputConfig pin V1.gpio_output_conf_pushpull)
    ]

-- | Configure pull-up/down reistor or no pullup for `GPIOPin`
pinSetPUPD :: GPIOPin -> V2.GPIO_PUPD -> Ivory eff ()
pinSetPUPD (GPIOFX pin) v2pupd = V2.pinSetPUPD pin v2pupd
pinSetPUPD (GPIOF1 pin) v2pupd = do
  v1mode <- V1.pinGetMode pin
  cond_
    [ v1mode ==? V1.gpio_mode_input ==>
       --input mode
       cond_
          [ v2pupd ==? V2.gpio_pupd_none     ==> V1.pinSetInputConfig pin V1.gpio_input_conf_float
          , v2pupd ==? V2.gpio_pupd_pullup   ==> V1.pinSetInputConfig pin V1.gpio_input_conf_pull_updown >> V1.pinSet pin
          , v2pupd ==? V2.gpio_pupd_pulldown ==> V1.pinSetInputConfig pin V1.gpio_input_conf_pull_updown >> V1.pinClear pin
          ]
    , true ==>
       -- output mode
       cond_
          [ v2pupd ==? V2.gpio_pupd_none ==> do

              af <- V1.pinIsAF pin
              ifte_ af
                (V1.pinSetOutputConfig pin V1.gpio_output_conf_af_opendrain)
                (V1.pinSetOutputConfig pin V1.gpio_output_conf_opendrain)

          , v2pupd ==? V2.gpio_pupd_pullup .|| v2pupd ==? V2.gpio_pupd_pulldown ==> do

              af <- V1.pinIsAF pin
              ifte_ af
                (V1.pinSetOutputConfig pin V1.gpio_output_conf_af_pushpull)
                (V1.pinSetOutputConfig pin V1.gpio_output_conf_pushpull)
          ]
    ]

-- | Alternative to `pinSetPUPD` using `GPIOPull` type
pinSetPull :: GPIOPin -> GPIOPull -> Ivory eff ()
pinSetPull p PullUp   = pinSetPUPD  p gpio_pupd_pullup
pinSetPull p PullDown = pinSetPUPD  p gpio_pupd_pulldown
pinSetPull _p NoPull   = return ()
