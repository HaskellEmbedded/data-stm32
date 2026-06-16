{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module {{ modns }}
  ( dacTower
  , module Ivory.BSP.STM32.Peripheral.DAC
  ) where

import Control.Monad (forM_)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.DAC
import Ivory.BSP.STM32.Peripheral.GPIO

dacTower
  :: DAC
  -> DACMode -- ^ Alignment and bit-width for both channels
  -> DACBuffering -- ^ Buffering for both channels
  -> GPIOPin -- ^ Channel 1 pin
  -> GPIOPin -- ^ Channel 2 pin
  -> Tower e
      ( ChanInput (Stored Uint16)
      , ChanInput (Stored Uint16)
      )
dacTower periph mode buffering channel1Pin channel2Pin = do
  dacChan1 <- channel
  dacChan2 <- channel

  monitor (named "PeripheralDriver") $ do
    monitorModuleDef $ hw_moduledef

    handler systemInit (named "Init") $ do
      callback $ const $ do
        forM_
          [ channel1Pin
          , channel2Pin
          ]
          $ \p -> do
            pinEnable        p
            pinSetMode       p gpio_mode_analog
            pinSetOutputType p gpio_outputtype_pushpull
            pinSetPUPD       p gpio_pupd_none
        dacInit periph

    c1Enabled <- state (named "Chan1Enabled")
    c2Enabled <- state (named "Chan2Enabled")

    handler (snd dacChan1) (named "Chan1") $ do
      callbackV $ \val -> do
        ena <- deref c1Enabled
        unless
          ena
          $ do
              dacEnableChannel periph DACChannel_1 buffering
              store c1Enabled true

        dacWrite
          periph
          DACChannel_1
          mode
          val

    handler (snd dacChan2) (named "Chan2") $ do
      callbackV $ \val -> do
        ena <- deref c2Enabled
        unless
          ena
          $ do
              dacEnableChannel periph DACChannel_2 buffering
              store c2Enabled true

        dacWrite
          periph
          DACChannel_2
          mode
          val

  pure
    ( fst dacChan1
    , fst dacChan2
    )
  where
  named nm = dacName periph <> nm
