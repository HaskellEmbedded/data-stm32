{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.BSP.STM32.Driver.ETH
  ( ethTower
  , module Ivory.BSP.STM32.Driver.ETH.FrameBuffer
  , module Ivory.BSP.STM32.Driver.ETH.RxPacket
  ) where

import Control.Monad (forM_)

import Ivory.HW
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface (BackpressureTransmit(..))

import qualified Ivory.BSP.ARMv7M.Instr
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.DMA.Artifacts (dmaArtifacts, dmaRefToUint32Header)
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.ETH
import Ivory.BSP.STM32.Peripheral.ETH.MAC.Regs
import Ivory.BSP.STM32.Peripheral.ETH.MAC.RegTypes
import Ivory.BSP.STM32.Peripheral.ETH.DMA.Regs
import Ivory.BSP.STM32.Peripheral.ETH.DMA.RegTypes
import Ivory.BSP.STM32.Peripheral.ETH.MMC.Regs

import Ivory.BSP.STM32.Driver.ETH.FrameBuffer
import Ivory.BSP.STM32.Driver.ETH.MIIMTypes
import Ivory.BSP.STM32.Driver.ETH.PhyState
import Ivory.BSP.STM32.Driver.ETH.RxPacket

-- | Ethernet peripheral driver
-- Tested on STM32F7 with LAN8742A (standard f7 nucleo board)
ethTower
  :: (e -> ClockConfig)
  -> ETHConfig
  -> Tower
       e
       ( ChanOutput ('Stored IBool)
       , BackpressureTransmit FrameBuffer ('Stored IBool)
       , ChanOutput (Struct "rx_packet")
       )
ethTower tocc ETHConfig{..} = do
  let
    ETH{..} = ethConfigPeriph
    ETHPins{..} = ethConfigPins
    named :: String -> String
    named = ("eth"++)

  mapM_ towerArtifact dmaArtifacts
  ethTowerDeps

  clockConfig <- fmap tocc getEnv

  ethIrq
    <- signalUnsafe
         (Interrupt $ ethInterrupt ethMAC)
         (Microseconds 10)
         (interrupt_disable (ethInterrupt ethMAC))

  (BackpressureTransmit miimReq miimRes)
    <- miimTower ethMAC

  phyPollPeriod <- period (Milliseconds 10)
  phyInit <- channel
  readyChan <- channel
  txReq <- channel
  txDone <- channel
  rxDone <- channel

  monitor (named "PeriphDriver") $ do
    periphInitDone <- state (named "PeriphInitDone")
    phyInitDone <- state (named "PhyInitDone")

    isFirstTDes <- stateInit (named "IsFirstTDes") (ival true)

    -- Transmit descriptors
    (tdes0 :: Ref 'Global ('Array 8 ('Stored Uint32)))
      <- state (named "Tdes0")
    (tdes1 :: Ref 'Global ('Array 8 ('Stored Uint32)))
      <- state (named "Tdes1")

    isFirstRDes <- stateInit (named "IsFirstRDes") (ival true)

    -- Receive descriptors
    (rdes0 :: Ref 'Global ('Array 8 ('Stored Uint32)))
      <- state (named "Rdes0")
    (rdes1 :: Ref 'Global ('Array 8 ('Stored Uint32)))
      <- state (named "Rdes1")

    -- Data buffers -> up to MTU -> up to 2^13 (8192)
    (tframe0 :: Ref 'Global FrameBuffer)
      <- state (named "Tframe0")
    (tframe1 :: Ref 'Global FrameBuffer)
      <- state (named "Tframe1")

    (rframe0 :: Ref 'Global FrameBuffer)
      <- state (named "Rframe0")
    (rframe1 :: Ref 'Global FrameBuffer)
      <- state (named "Rframe1")

    (rxPacket :: Ref Global (Struct "rx_packet"))
      <- state (named "RxPacket")

    monitorModuleDef $ do
      hw_moduledef
      Ivory.BSP.ARMv7M.Instr.instrModuleDef
      incl refU8_to_uint32_proc
      incl refU32_to_uint32_proc

    handler systemInit (named "Init") $ do
      phyInitE <- emitter (fst phyInit) 1
      callback $ const $ do
        ethSetRMII

        forM_
          [ ethPins_mdc
          , ethPins_mdio
          , ethPins_refclk
          , ethPins_crs
          , ethPins_txen
          , ethPins_txd0
          , ethPins_txd1
          , ethPins_rxd0
          , ethPins_rxd1
          ]
          $ \p -> do
            pinEnable        p
            pinSetOutputType p gpio_outputtype_pushpull
            pinSetPUPD       p gpio_pupd_none
            pinSetAF         p (ethAFLookup p)
            pinSetMode       p gpio_mode_af

        ethRCCEnable ethMAC
        ethRCCRXEnable ethMAC
        ethRCCTXEnable ethMAC

        dmaInit
          ethDMA
        macInit
          clockConfig
          ethMAC
        mmcInit
          ethMMC

        -- Rx descriptors
        -- 1st
        store
          (rdes0 ! 0)
          $ withBits 0
          $ do
              setField eth_rdes0_own owned_by_dma

        store
          (rdes0 ! 1)
          $ withBits 0
          $ do
              setBit eth_rdes1_rch -- Second address chained
              setField eth_rdes1_rbs1 -- Receive buffer 1 size
                (fromRep $ arrayLen (rframe0 ~> stringDataL))

        rFrame0Addr <-
          call
            refU8_to_uint32_proc
            (rframe0 ~> stringDataL ! 0)

        store
          (rdes0 ! 2)
          rFrame0Addr

        -- 2nd
        store
          (rdes1 ! 0)
          $ withBits 0
          $ do
              setField eth_rdes0_own owned_by_dma

        store
          (rdes1 ! 1)
          $ withBits 0
          $ do
              setBit eth_rdes1_rer -- Receive end of ring
              setBit eth_rdes1_rch -- Second address chained
              setField eth_rdes1_rbs1 -- Receive buffer 1 size
                (fromRep $ arrayLen (rframe1 ~> stringDataL))

        rFrame1Addr <-
          call
            refU8_to_uint32_proc
            (rframe1 ~> stringDataL ! 0)

        store
          (rdes1 ! 2)
          rFrame1Addr

        -- chain rdes0 -> rdes1
        nextRdesAddr <-
          call
            refU32_to_uint32_proc
            (rdes1 ! 0)

        store
          (rdes0 ! 3)
          nextRdesAddr

        call_ Ivory.BSP.ARMv7M.Instr.dsb

        -- ring
        r_list_start_addr <-
          call
            refU32_to_uint32_proc
            (rdes0 ! 0)

        -- Set list start address
        modifyReg (ethRegDMARDLAR ethDMA)
          $ setField eth_dmardlar_srl
          $ fromRep r_list_start_addr

        -- Start reception
        modifyReg (ethRegDMAOMR ethDMA)
          $ setBit eth_dmaomr_sr

        nextTdesAddr <-
          call
            refU32_to_uint32_proc
            (tdes1 ! 0)

        store
          (tdes0 ! 3)
          nextTdesAddr

        list_start_addr <-
          call
            refU32_to_uint32_proc
            (tdes0 ! 0)

        call_ Ivory.BSP.ARMv7M.Instr.dsb

        -- Set list start address
        modifyReg (ethRegDMATDLAR ethDMA)
          $ setField eth_dmatdlar_stl
          $ fromRep list_start_addr

        -- Start transmission
        modifyReg (ethRegDMAOMR ethDMA)
          $ setBit eth_dmaomr_st


        interrupt_enable (ethInterrupt ethMAC)

        store periphInitDone true
        emitV phyInitE true

    phyState <- stateInit (named "PhyState") (ival phyUnknown)

    miimBcr <- state (named "MiimBcr")
    miimBsr <- state (named "MiimBsr")
    miimPhysId <- stateInit (named "MiimPhysId") (ival (0 :: Uint32))
    miimSsr <- stateInit (named "MiimSsr") (ival (0 :: Uint16))

    isLAN87xxa <- state (named "IsLAN87xxa")

    handler phyPollPeriod (named "PhyPoll") $ do
      readyE <- emitter (fst readyChan) 1
      reqE <- emitter miimReq 1
      callback $ const $ do
        let
          phyWriteReq reg regData = do
            req <- local $ istruct []
            store (req ~> miim_request_register) reg
            store (req ~> miim_request_write) true
            store
              (req ~> miim_request_data)
              regData
            emit reqE (constRef req)

          phyReadReq reg  = do
            req <- local $ istruct []
            store (req ~> miim_request_register) reg
            emit reqE (constRef req)

        ethInitDone <- deref periphInitDone
        initDone <- deref phyInitDone
        when (ethInitDone .&& (iNot initDone)) $ do
          pstate <- deref phyState
          cond_
            [ pstate ==? phyUnknown ==> do
                phyWriteReq
                  phy_register_bcr
                  $ withBits 0 $ setBit phy_bcr_reset
                store phyState phyResetReq
            , pstate ==? phyReset ==> do
                phyReadReq phy_register_bcr
                store phyState phyResetClearedReq
            , pstate ==? phyResetCleared ==> do
                phyReadReq phy_register_phys_id1
                store phyState phyPhysId1Req
            , pstate ==? phyPhysId1 ==> do
                phyReadReq phy_register_phys_id2
                store phyState phyPhysId2Req
            -- has to be polled repeatadly
            , pstate ==? phyPhysId2 .|| pstate ==? phyLinkWaitReq ==> do
                phyReadReq phy_register_bsr
                store phyState phyLinkWaitReq
            , pstate ==? phyLinkWait ==> do
                comment "Enable and restart auto negotiation"
                m0 <- deref miimBcr
                phyWriteReq
                  phy_register_bcr
                  $ withBits m0
                  $ do
                    setBit phy_bcr_autoneg_enable
                    setBit phy_bcr_autoneg_restart

                store phyState phyAutonegStartReq
            -- has to be polled repeatadly
            , pstate ==? phyAutonegStart .|| pstate ==? phyAutonegReq ==> do
                phyReadReq phy_register_bsr
                store phyState phyAutonegReq
            , pstate ==? phyAutoneg ==> do
                phyReadReq phy_register_ssr
                store phyState phySsrReq
            , pstate ==? phyDone ==> do
                store phyInitDone true
                emitV readyE true
            ]

    handler miimRes (named "MiimRes") $ do
      callback $ \res -> do
        pstate <- deref phyState
        isOkay <- res ~>* miim_result_okay
        assert isOkay

        cond_
          [ pstate ==? phyResetReq ==> do
              store phyState phyReset
          , pstate ==? phyResetClearedReq ==> do
              bcr <- res ~>* miim_result_data
              -- Reset bit should be cleared when done
              unless
                (bitToBool
                  (fromRep bcr #. phy_bcr_reset)
                )
                $ do
                    store miimBcr bcr
                    store phyState phyResetCleared
          , pstate ==? phyPhysId1Req ==> do
              miimId1 <- deref (res ~> miim_result_data)
              miimPhysId += ((safeCast miimId1) `iShiftL` 16)
              store phyState phyPhysId1
          , pstate ==? phyPhysId2Req ==> do
              miimId2 <- deref (res ~> miim_result_data)
              miimPhysId += safeCast miimId2

              -- LAN87xxa has 0xC1310007 identifier
              physId <- deref miimPhysId
              when
                (physId ==? 0x0007_C131)
                $ store isLAN87xxa true

              store phyState phyPhysId2

          , pstate ==? phyLinkWaitReq ==> do
              bsr <- res ~>* miim_result_data
              store miimBsr bsr
              when
                (bitToBool
                  (fromRep bsr #. phy_bsr_link_up)
                )
                $ do
                  refCopy miimBsr (res ~> miim_result_data)
                  store phyState phyLinkWait

          , pstate ==? phyAutonegStartReq ==> do
              store phyState phyAutonegStart

          , pstate ==? phyAutonegReq ==> do
              bsr <- res ~>* miim_result_data
              refCopy miimBsr (res ~> miim_result_data)
              when
                (bitToBool
                  (fromRep bsr #. phy_bsr_autoneg_complete)
                )
                $ do
                  store phyState phyAutoneg
          , pstate ==? phySsrReq ==> do
              ssr <- fromRep <$> res ~>* miim_result_data
              setSpeed
                ethMAC
                (bitToBool $ ssr #. phy_ssr_100mbit)
                (bitToBool $ ssr #. phy_ssr_full_duplex)
              refCopy miimSsr (res ~> miim_result_data)
              store phyState phyDone
          ]

    handler (snd txReq) (named "TxReq") $ do
      callback $ \txBuf -> do
        fstDesc <- deref isFirstTDes
        store isFirstTDes (iNot fstDesc)

        tdes <- assign $ fstDesc ? (tdes0, tdes1)
        tframe <- assign $ fstDesc ? (tframe0, tframe1)
        refCopy tframe txBuf

        store
          (tdes ! 0)
          $ withBits 0
          $ do
              setField eth_tdes0_own owned_by_dma
              setBit eth_tdes0_ic -- Interrupt on completion
              setBit eth_tdes0_fs -- First segment
              setBit eth_tdes0_ls -- Last segment
              setField eth_tdes0_cic cic_all
              setBit eth_tdes0_tch -- Transmit chained
              setField eth_tdes0_ter (boolToBit (iNot fstDesc))

        frameLen <-
          ((bitCast :: Uint32 -> Uint16) . signCast)
           <$>
          deref (tframe ~> stringLengthL)

        frameAddr <-
          call
            refU8_to_uint32_proc
            (tframe ~> stringDataL ! 0)

        -- set frame length and buffer pointer
        store
          (tdes ! 1)
          $ withBits 0
          $ do
              setField eth_tdes1_tbs1 (fromRep frameLen)

        store
          (tdes ! 2)
          frameAddr

        call_ Ivory.BSP.ARMv7M.Instr.dsb

        -- Demand poll
        modifyReg (ethRegDMATPDR ethDMA)
          $ setField eth_dmatpdr_tpd (fromRep 0)

    rxErrors <- stateInit (named "RxErrors") (ival (0 :: Uint32))
    rxFragmented <- stateInit (named "RxFragmented") (ival (0 :: Uint32))

    handler ethIrq (named "IsrHandler") $ do
      txDoneE <- emitter (fst txDone) 1
      rxDoneE <- emitter (fst rxDone) 1
      callback $ const $ do
        sr <- getReg $ (ethRegDMASR ethDMA)
        when (bitToBool (sr #. eth_dmasr_ts)) $ do
          setReg (ethRegDMASR ethDMA) $ do
            -- Clear normal interrupt summary
            setBit eth_dmasr_nis
            -- Clear transmit status
            setBit eth_dmasr_ts

          emitV txDoneE true

        when (bitToBool (sr #. eth_dmasr_rs)) $ do
          setReg (ethRegDMASR ethDMA) $ do
            -- Clear normal interrupt summary
            setBit eth_dmasr_nis
            -- Clear receive status
            setBit eth_dmasr_rs

          fstDesc <- deref isFirstRDes
          store isFirstRDes (iNot fstDesc)

          rdes <- assign $ fstDesc ? (rdes0, rdes1)
          rframe <- assign $ fstDesc ? (rframe0, rframe1)

          rd0 <- fromRep <$> deref (rdes ! 0)
          rd4 <- fromRep <$> deref (rdes ! 4)

          store (rxPacket ~> rx_packet_error) false
          store (rxPacket ~> rx_packet_fragmented) false
          store (rxPacket ~> rx_packet_overflow) false
          store (rxPacket ~> rx_packet_crc_error) false
          store (rxPacket ~> rx_packet_receive_error) false
          store (rxPacket ~> rx_packet_ip_error) false

          when
            (bitToBool (rd0 #. eth_rdes0_es))
            $ do
                rxErrors += 1
                store (rxPacket ~> rx_packet_error) true

          when
            (bitToBool (rd0 #. eth_rdes0_oe))
            $ store (rxPacket ~> rx_packet_overflow) true

          when
            (bitToBool (rd0 #. eth_rdes0_ce))
            $ store (rxPacket ~> rx_packet_crc_error) true

          when
            (bitToBool (rd0 #. eth_rdes0_re))
            $ store (rxPacket ~> rx_packet_receive_error) true

          when
            (    (bitToBool (rd4 #. eth_rdes4_iphe)) -- IP Header error
             .|| (bitToBool (rd4 #. eth_rdes4_ippe)) -- IP Payload error
            )
            $ store (rxPacket ~> rx_packet_ip_error) true

          when
            ((bitToBool (rd0 #. eth_rdes0_fs))
              .&& iNot (bitToBool (rd0 #. eth_rdes0_ls)))
            $ do
                rxFragmented += 1
                store (rxPacket ~> rx_packet_fragmented) true

          -- Set frame length (including padding)
          store
            (rframe ~> stringLengthL)
            $ safeCast $ toRep (rd0 #. eth_rdes0_fl)

          store
            (rdes ! 0)
            $ withBits 0
            $ do
                setField eth_rdes0_own owned_by_dma

          refCopy (rxPacket ~> rx_packet_buffer) rframe
          emit rxDoneE (constRef rxPacket)

        interrupt_enable (ethInterrupt ethMAC)

  pure
    ( snd readyChan
    , BackpressureTransmit (fst txReq) (snd txDone)
    , snd rxDone
    )
  where
    ethTypes :: Module
    ethTypes = package "ethTypes" $ do
      defStringType (Proxy :: Proxy FrameBuffer)
      defStruct (Proxy :: Proxy "rx_packet")

    ethTowerDeps :: Tower e ()
    ethTowerDeps = do
      towerDepends ethTypes
      towerModule ethTypes

-- | Init ETH MAC peripheral
macInit
  :: ( GetAlloc eff ~ 'Scope c )
  => ClockConfig
  -> MAC
  -> Ivory eff ()
macInit cc MAC{..} = do
  -- MII address register
  modifyReg ethRegMACMIIAR $
   setField
    eth_macmiiar_cr
    $ case clockHClkHz cc of
        x |                     x <  20_000_000 -> error "Wrong HCLK clock, ETH needs over 20Mhz"
        x | x >=  20_000_000 && x <  35_000_000 -> clock_range_hclk_div16
        x | x >=  35_000_000 && x <  60_000_000 -> clock_range_hclk_div26
        x | x >=  60_000_000 && x < 100_000_000 -> clock_range_hclk_div42
        x | x >= 100_000_000 && x < 150_000_000 -> clock_range_hclk_div62
        x | x >= 150_000_000                    -> clock_range_hclk_div102
        _ | otherwise                           -> error "absurd"

  -- Config register
  modifyReg ethRegMACCR $ do
    -- CRC stripping for Type frames
    setBit eth_maccr_cstf
    -- Fat ethernet mode
    setBit eth_maccr_fes
    -- Duplex mode
    setBit eth_maccr_dm
    -- IPv4 checksum offload
    setBit eth_maccr_ipco
    -- Automatic pad/CRC stripping
    setBit eth_maccr_apcs
    -- Retry disable
    setBit eth_maccr_rd
    -- Reciever enable
    setBit eth_maccr_re
    -- Transmiter enable
    setBit eth_maccr_te

  -- Frame filter register
  modifyReg ethRegMACFFR $ do
    -- Receive all
    setBit eth_macffr_ra
    -- Promiscuous mode
    setBit eth_macffr_pm

  -- Flow Control Register
  modifyReg ethRegMACFCR $ do
    -- Pause time
    setField eth_macfcr_pt (fromRep 0x100)
    -- Disable zero quanta pause
    setBit eth_macfcr_zqpd

-- | Set speed based on e.g. auto negotiation result
setSpeed
  :: ( GetAlloc eff ~ 'Scope c )
  => MAC
  -> IBool -- ^ Is 100Mbit
  -> IBool -- ^ Is full duplex
  -> Ivory eff ()
setSpeed MAC{..} is100Mbit isFullDuplex = do
  -- Config register
  modifyReg ethRegMACCR $ do
    -- Fat ethernet mode
    setField eth_maccr_fes (boolToBit is100Mbit)
    -- Duplex mode
    setField eth_maccr_dm (boolToBit isFullDuplex)

-- | Init ETH MMC peripheral
mmcInit
  :: ( GetAlloc eff ~ 'Scope c )
  => MMC
  -> Ivory eff ()
mmcInit MMC{..} = do
  -- Disable MMC RX interrupts
  modifyReg ethRegMMCRIMR $ do
    setBit eth_mmcrimr_rgufm
    setBit eth_mmcrimr_rfaem
    setBit eth_mmcrimr_rfcem

  -- Disable MMC TX interrupts
  modifyReg ethRegMMCTIMR $ do
    setBit eth_mmctimr_tgfm
    setBit eth_mmctimr_tgfmscm
    setBit eth_mmctimr_tgfscm

-- | Init ETH DMA peripheral
dmaInit
  :: ( GetAlloc eff ~ 'Scope c
     , GetBreaks (AllowBreak eff) ~ 'Break
     )
  => ETHDMA
  -> Ivory eff ()
dmaInit ETHDMA{..} = do
  -- Operation mode register
  modifyReg ethRegDMAOMR $ do
    -- Disable dropping of TCP/IP checksum error frames
    setBit eth_dmaomr_dtcefd
    -- Disable flushing of received frames
    setBit eth_dmaomr_dfrf
    -- Receive store and forward
    setBit eth_dmaomr_rsf
    -- Transmit store and forward
    setBit eth_dmaomr_tsf
    -- Forward error frames
    setBit eth_dmaomr_fef
    -- Operate on second frame
    setBit eth_dmaomr_osf

  -- Bus mode register
  modifyReg ethRegDMABMR $ do
    -- Enhanced descriptor format enable
    setBit eth_dmabmr_edfe
    -- Address-aligned beats
    setBit eth_dmabmr_aab
    -- Fixed burst
    setBit eth_dmabmr_fb
    -- Programmable burst length
    setField eth_dmabmr_rdp (fromRep 32)
    setField eth_dmabmr_pbl (fromRep 32)
    -- RX:TX ratio 2:1
    setField eth_dmabmr_pm rx_tx_ratio2to1

  -- Interrupt enable register
  modifyReg ethRegDMAIER $ do
    -- Normal interrupt summary enable
    setBit eth_dmaier_nise
    -- Receive interrupt enable
    setBit eth_dmaier_rie
    -- Transmit interrupt enable
    setBit eth_dmaier_tie

-- | MIIM (MDIO) PHY communication handling via ETH MAC peripheral
miimTower
  :: MAC
  -> Tower
       e
       ( BackpressureTransmit
           (Struct "miim_request")
           (Struct "miim_result")
       )
miimTower MAC{..} = do
  towerDepends miimTypes
  towerModule miimTypes

  (backpressureTransmit, miimRequest) <- channel
  (miimReply, backpressureComplete) <- channel

  per <- period (Milliseconds 1)

  monitor (named "Driver") $ do
    timeouts <- stateInit (named "Timeouts") (ival (0 :: Uint8))
    timeoutCounter <- stateInit (named "TimeoutCounter") $ ival (0 :: Uint16)
    inProgress <- state (named "InProgress")
    request <- state (named "Request")
    result <- state (named "Result")

    handler miimRequest (named "Request") $ do
      callback $ \req -> do
        running <- deref inProgress
        assert (iNot running)

        store inProgress true
        store timeoutCounter 0

        refCopy request req
        reset <- local $ istruct []
        refCopy result reset

        phyAddr <- req ~>* miim_request_address
        miiReg <- req ~>* miim_request_register
        val <- req ~>* miim_request_data
        isWrite <- req ~>* miim_request_write

        -- MAC MII data register
        modifyReg ethRegMACMIIDR $
          -- MII data read from/written to the PHY
          setField eth_macmiidr_md $ fromRep val

        -- MAC MII address register
        modifyReg ethRegMACMIIAR $ do
          -- PHY address (0-32)
          setField eth_macmiiar_pa (fromRep phyAddr)
          -- MII register (0-32)
          setField eth_macmiiar_mr (fromRep $ toRep miiReg)
          -- Read or write operation
          setField eth_macmiiar_mw (boolToBit isWrite)
          -- Set busy
          setBit eth_macmiiar_mb

    handler per (named "Period") $ do
      resE <- emitter miimReply 1
      callback $ const $ do
        running <- deref inProgress
        when running $ do
          bmr <- getReg ethRegMACMIIAR
          when
            (bitToBool (bmr #. eth_macmiiar_mb) ==? false)
            $ do
                store (result ~> miim_result_okay) true
                store inProgress false

                isWrite <- request ~>* miim_request_write
                unless isWrite $ do
                  -- MAC MII data register
                  dr <- getReg ethRegMACMIIDR
                  -- MII data read from/written to the PHY
                  store
                    (result ~> miim_result_data)
                    $ toRep (dr #. eth_macmiidr_md)

                emit resE (constRef result)

          timeoutCounter += 1
          tc <- deref timeoutCounter
          when (tc ==? (2 ^ (16 :: Int)) - 1) $ do
            timeouts += 1
            emit resE (constRef result)
            store inProgress false

  pure $ BackpressureTransmit{..}
  where
    named :: String -> String
    named = ("miim"++)

    miimTypes :: Module
    miimTypes = package "miimTypes" $ do
      defStruct (Proxy :: Proxy "miim_request")
      defStruct (Proxy :: Proxy "miim_result")

refU8_to_uint32_proc :: Def('[Ref s ('Stored Uint8)] :-> Uint32)
refU8_to_uint32_proc = importProc "ref_to_uint32" dmaRefToUint32Header

refU32_to_uint32_proc :: Def('[Ref s ('Stored Uint32)] :-> Uint32)
refU32_to_uint32_proc = importProc "ref_to_uint32" dmaRefToUint32Header
