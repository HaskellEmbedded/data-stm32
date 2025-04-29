{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.BSP.STM32.Peripheral.ETH.DMA.RegTypes where

import Ivory.Language

[ivory|
  -- BMR.PM
  bitdata RxTxRatio :: Bits 2
    = rx_tx_ratio1to1 as 0b00
    | rx_tx_ratio2to1 as 0b01
    | rx_tx_ratio3to1 as 0b10
    | rx_tx_ratio4to1 as 0b11

  -- SR.TPS
  bitdata TransmitProcessState :: Bits 3
    = transmit_process_state_stopped          as 0b000 -- Reset or Stop Transmit Command issued
    | transmit_process_state_running_fetching as 0b001 -- Fetching transmit transfer descriptor
    | transmit_process_state_running_waiting  as 0b010 -- Waiting for status
    | transmit_process_state_running_reading  as 0b011 -- Reading Data from host memory buffer and queuing it to transmit buffer (Tx FIFO)
    | transmit_process_state_reserved1        as 0b100
    | transmit_process_state_reserved2        as 0b101
    | transmit_process_state_suspended        as 0b110 -- Transmit descriptor unavailable or transmit buffer underflow
    | transmit_process_state_running_closing  as 0b111 -- Closing transmit descriptor

  -- SR.RPS
  bitdata ReceiveProcessState :: Bits 3
    = receive_process_state_stopped          as 0b000 -- Reset or Stop Receive Command issued
    | receive_process_state_running_fetching as 0b001 -- Fetching receive transfer descriptor
    | receive_process_state_reserved1        as 0b010
    | receive_process_state_running_waiting  as 0b011 -- Waiting for receive packet
    | receive_process_state_suspended        as 0b100 -- Receive descriptor unavailable
    | receive_process_state_running_closing  as 0b101 -- Closing receive descriptor
    | receive_process_state_reserved2        as 0b110
    | receive_process_state_running_transfer as 0b111 -- Transferring the receive packet data from receive buffer to host memory
|]

