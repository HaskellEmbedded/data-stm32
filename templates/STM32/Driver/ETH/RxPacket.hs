-- | RX packet response struct
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32.Driver.ETH.RxPacket where

import Ivory.Language
import Ivory.BSP.STM32.Driver.ETH.FrameBuffer

[ivory|
  struct rx_packet
    { rx_packet_buffer        :: FrameBuffer
    ; rx_packet_error         :: Stored IBool -- ^ Error summary
    ; rx_packet_fragmented    :: Stored IBool -- ^ Fragmented packet received
    ; rx_packet_overflow      :: Stored IBool -- ^ Buffer overflow
    ; rx_packet_crc_error     :: Stored IBool -- ^ CRC error
    ; rx_packet_receive_error :: Stored IBool -- ^ Receive error (indicated by PHY)
    ; rx_packet_ip_error      :: Stored IBool -- ^ IP header or payload error (IPHE IPPE)
    }
|]
