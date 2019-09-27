{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module {{ modns }}
  ( i2cTower
  , module Ivory.Tower.HAL.Bus.I2C
  , module Ivory.Tower.HAL.Bus.I2C.DeviceAddr
  ) where

import Control.Monad (void)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr
import Ivory.Tower.HAL.Bus.Interface
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.I2C.Pins
import Ivory.BSP.STM32.Peripheral.I2Cv2.Regs
import Ivory.BSP.STM32.Peripheral.I2Cv2.Peripheral

-- | Max number of failed transactions in a row before we try to reset
-- the peripheral
i2c_MAX_ERROR_RUN :: Uint32
i2c_MAX_ERROR_RUN = 5

i2cTower :: (e -> ClockConfig)
         -> I2C
         -> I2CPins
         -> Tower e ( BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
                    , ChanOutput ('Stored ITime))
i2cTower tocc periph I2CPins{..} = do
  towerDepends i2cTowerTypes
  towerModule  i2cTowerTypes
  reqchan <- channel
  reschan <- channel
  readychan <- channel
  ready_per <- period (Milliseconds 1)
  watchdog_per <- period (Milliseconds 20)
  evt_irq <- signalUnsafe
                (Interrupt (i2cIntEvent periph))
                (Microseconds 50)
                (interrupt_disable (i2cIntEvent periph))

  err_irq <- signalUnsafe
                (Interrupt (i2cIntError periph))
                (Microseconds 50)
                (interrupt_disable (i2cIntError periph))
  monitor ((i2cName periph) ++ "PeripheralDriver") $
    i2cPeripheralDriver tocc periph i2cpins_sda i2cpins_scl evt_irq err_irq
      (snd reqchan) (fst reschan) ready_per (fst readychan) watchdog_per
  return (BackpressureTransmit (fst reqchan) (snd reschan), snd readychan)

i2cTowerTypes :: Module
i2cTowerTypes = package "i2cTowerTypes" $ do
  defStruct (Proxy :: Proxy "i2c_transaction_request")
  defStruct (Proxy :: Proxy "i2c_transaction_result")


i2cPeripheralDriver :: forall e
                     . (e -> ClockConfig)
                    -> I2C
                    -> GPIOPin
                    -> GPIOPin
                    -> ChanOutput ('Stored ITime)
                    -> ChanOutput ('Stored ITime)
                    -> ChanOutput ('Struct "i2c_transaction_request")
                    -> ChanInput  ('Struct "i2c_transaction_result")
                    -> ChanOutput ('Stored ITime)
                    -> ChanInput  ('Stored ITime)
                    -> ChanOutput ('Stored ITime)
                    -> Monitor e ()
i2cPeripheralDriver tocc periph sda scl evt_irq err_irq req_chan res_chan ready_per ready_in watchdog_per = do
  clockConfig <- fmap tocc getEnv
  monitorModuleDef $ hw_moduledef

  let named nm = i2cName periph ++ "_" ++ nm

  handler systemInit (named "init") $ do
    callback $ const $ do
      i2cInit        periph sda scl clockConfig
      interrupt_enable (i2cIntEvent periph)
      interrupt_enable (i2cIntError periph)

  ready_sent <- state (named "ready_sent")
  handler ready_per (named "ready_period") $ do
    send_ready <- emitter ready_in 1
    callback $ \now -> do
      r <- deref ready_sent
      unless r $ emit send_ready now
      store ready_sent true

  (reqbuffer :: Ref 'Global ('Struct "i2c_transaction_request"))
    <- state (named "reqbuffer")
  (reqbufferpos :: Ref 'Global ('Stored (Ix 128)))
    <- state (named "reqbufferpos")

  (resbuffer :: Ref 'Global ('Struct "i2c_transaction_result"))
    <- state (named "resbuffer")
  (resbufferpos :: Ref 'Global ('Stored (Ix 128)))
    <- state (named "resbufferpos")

  (invalid_request :: Ref 'Global ('Stored Uint32))
    <- state (named "invalid_request")

  -- number of errors we've seen in a row
  (errorRun :: Ref 'Global ('Stored Uint32))
    <- state (named "errorRun")

  (successRun :: Ref 'Global ('Stored Uint32))
    <- state (named "successRun")

  -- time of last event
  (last_event :: Ref 'Global ('Stored ITime))
    <- state (named "last_event")

  -- transaction pending
  (pending :: Ref 'Global ('Stored IBool))
    <- state (named "pending")

  -- debugging vars
  (busError :: Ref 'Global ('Stored IBool))
    <- state (named "busError")

  (nackError :: Ref 'Global ('Stored IBool))
    <- state (named "nackError")

  (arbitrationLostError :: Ref 'Global ('Stored IBool))
    <- state (named "arbitrationLostErrorr")

  (unknownError :: Ref 'Global ('Stored IBool))
    <- state (named "unknownError")

  (watchdogTriggers :: Ref 'Global ('Stored Uint32))
    <- state (named "watchdogTriggers")


  let sendresult :: Emitter ('Struct "i2c_transaction_result")
                 -> Ref s' ('Struct "i2c_transaction_result")
                 -> Uint8
                 -> Ivory eff ()
      sendresult e res code = do

          -- shut down interrupts until the
          -- next request comes in
          disableCRInterrupts periph

          -- Don't send a result when inactive
          isPending <- deref pending
          when (isPending) $ do
            store pending false
            -- keep track of errors, resetting the run of errors
            -- whenever we have a successful result to send
            store (res ~> resultcode) code
            ifte_ (code >? 0)
              (do errorRun += 1
                  store successRun 0)
              (do successRun += 1
                  store errorRun 0)
            emit e (constRef res)

  let initTransfer :: Ivory (AllocEffects s) ()
      initTransfer = do
            tx_pos <- deref reqbufferpos
            tx_sz  <- deref (reqbuffer ~> tx_len)
            rx_pos <- deref resbufferpos
            rx_sz  <- deref (reqbuffer ~> rx_len)

            let write_remaining = tx_sz - tx_pos
                read_remaining  = rx_sz - rx_pos

            tx_ad  <- deref (reqbuffer ~> tx_addr)

            isTX <- assign (write_remaining >? 0)
            isRX <- assign (read_remaining  >? 0)
            assert (isTX .|| isRX)

            cond_ [
                isTX ==> do
                  modifyReg (i2cRegCR1 periph) $ do
                    setBit i2c_cr1_txie
                    clearBit i2c_cr1_rxie

                  modifyReg (i2cRegCR2 periph) $ do
                    setField i2c_cr2_nbytes (fromRep $ ixToU8 tx_sz)
                    clearBit i2c_cr2_rd_wrn

              , isRX ==> do
                  modifyReg (i2cRegCR1 periph) $ do
                    setBit i2c_cr1_rxie
                    clearBit i2c_cr1_txie

                  modifyReg (i2cRegCR2 periph) $ do
                    setField i2c_cr2_nbytes (fromRep $ ixToU8 rx_sz)
                    setBit   i2c_cr2_rd_wrn
              ]

            modifyReg (i2cRegCR2 periph) $ do
              -- sadd1 is Bits 7, lowest sadd0 is ignored
              -- and read/write is set by i2c_cr2_rd_wrn
              -- (we can use readAddr or writeAddr as it doesn't matter)
              setField i2c_cr2_sadd1 (fromRep $ (writeAddr tx_ad) `iShiftR` 1)

            setStart periph


  handler watchdog_per (named "watchdog") $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      errs <- deref errorRun

      when (errs >=? i2c_MAX_ERROR_RUN) $ do
        isPending <- deref pending
        when (isPending) $ do
          -- shoot down any in-progress transaction
          sendresult res_emitter resbuffer 1

        -- reset the peripheral
        disableCRInterrupts periph
        i2cReset periph sda scl clockConfig

        -- reset the error run
        store errorRun 0
        watchdogTriggers += 1

  handler err_irq (named "error_irq") $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      isr <- getReg (i2cRegISR periph)

      cond_
        [ (bitToBool (isr #. i2c_isr_berr)) ==> do
            comment "Bus error"
            store busError true
            modifyReg (i2cRegICR periph) $ setBit i2c_icr_berrcf

        , (bitToBool (isr #. i2c_isr_arlo)) ==> do
            comment "Arbitration lost"
            store arbitrationLostError true
            modifyReg (i2cRegICR periph) $ setBit i2c_icr_arlocf

        , true ==> do
            store unknownError true
        ]

      sendresult res_emitter resbuffer 2

      interrupt_enable (i2cIntError periph)

  handler evt_irq (named "event_irq") $ do
    res_emitter <- emitter res_chan 1
    callback $ \_ -> do
      store last_event =<< getTime
      isr <- getReg (i2cRegISR periph)

      tx_pos <- deref reqbufferpos
      tx_sz  <- deref (reqbuffer ~> tx_len)
      rx_pos <- deref resbufferpos
      rx_sz  <- deref (reqbuffer ~> rx_len)

      let write_remaining = tx_sz - tx_pos
          read_remaining  = rx_sz - rx_pos

      when (bitToBool (isr #. i2c_isr_txis)) $ do
        comment "TX empty, write new data"
        w <- deref ((reqbuffer ~> tx_buf) ! tx_pos)
        store reqbufferpos (tx_pos + 1)
        modifyReg (i2cRegTXDR periph) $
          setField i2c_txdr_txdata (fromRep w)

      when (bitToBool (isr #. i2c_isr_rxne)) $ do
        comment "RX non-empty, read data"
        dr <- getReg (i2cRegRXDR periph)
        r  <- assign (toRep (dr #. i2c_rxdr_rxdata))
        store ((resbuffer ~> rx_buf) ! rx_pos) r
        store resbufferpos (rx_pos + 1)

      when (bitToBool (isr #. i2c_isr_nackf)) $ do
        comment "NACK received"
        store nackError true
        modifyReg (i2cRegICR periph) $ setBit i2c_icr_nackcf

        sendresult res_emitter resbuffer 1

      when (bitToBool (isr #. i2c_isr_stopf)) $ do
        comment "STOP condition generated"
        modifyReg (i2cRegICR periph) $ setBit i2c_icr_stopcf

        comment "we should be at the end of transaction"
        assert (tx_pos ==? tx_sz)
        assert (rx_pos ==? rx_sz)

        disableCRInterrupts periph

        sendresult res_emitter resbuffer 0

      when (bitToBool (isr #. i2c_isr_tc)) $ do
        comment "Transfer complete"

        rx_pos <- deref resbufferpos
        let read_rem  = rx_sz - rx_pos

        ifte_ (read_rem >? 0)
          (initTransfer) -- repeated start
          (setStop periph)

      interrupt_enable (i2cIntEvent periph)

  handler req_chan (named "request") $ do
    res_emitter <- emitter res_chan 1
    callback $ \req -> do
      ready <- deref ready_sent
      isPending <- deref pending
      cond_
        [ iNot isPending ==> do
            store pending true
            refCopy reqbuffer req
            store reqbufferpos 0
            store resbufferpos 0
            store last_event =<< getTime

            initTransfer
            enableCRInterrupts periph
        , true ==> do
            invalid_request %= (+1)
            -- this previously used to forcefully emit result even tho
            -- there was no pending transaction, we should just assert instead
            -- as it means ready is not respected correctly by the dev
            assert(ready ==? true)
            sendresult res_emitter resbuffer 1
        ]

setStart :: I2C -> Ivory (AllocEffects s) ()
setStart periph = modifyReg (i2cRegCR2 periph) $ setBit i2c_cr2_start

setStop :: I2C -> Ivory (AllocEffects s) ()
setStop periph = modifyReg (i2cRegCR2 periph) $ setBit i2c_cr2_stop

enableCRInterrupts :: I2C -> Ivory eff ()
enableCRInterrupts periph = do
  modifyReg (i2cRegCR1 periph)
    (mapM_ setBit [
        i2c_cr1_errie
      , i2c_cr1_tcie
      , i2c_cr1_stopie
      , i2c_cr1_nackie
    ])

disableCRInterrupts :: I2C -> Ivory eff ()
disableCRInterrupts periph = do
  modifyReg (i2cRegCR1 periph)
    (mapM_ clearBit [
        i2c_cr1_errie
      , i2c_cr1_tcie
      , i2c_cr1_stopie
      , i2c_cr1_nackie
      , i2c_cr1_rxie
      , i2c_cr1_txie
    ])

ixToU8 :: ANat n => Ix n -> Uint8
ixToU8 = (bitCast :: Uint32 -> Uint8) . signCast . fromIx
