-- | Ethernet descriptor register types
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.BSP.STM32.Peripheral.ETH.Descriptor.Types where

import Ivory.Language

[ivory|
  bitdata Owned :: Bit
    = owned_by_cpu as 0
    | owned_by_dma as 1

  bitdata CIC :: Bits 2
    = cic_disabled as 0b00
    | cic_header_only as 0b01
    -- ^ Only IP header checksum calculation and insertion are enabled
    | cic_all_but_pseudo as 0b10
    -- ^ IP header checksum and payload checksum calculation
    -- and insertion are enabled, but pseudo-header checksum
    -- is not calculated in hardware
    | cic_all as 0b11
    -- ^ IP Header checksum and payload checksum calculation
    -- and insertion are enabled, and
    -- pseudo-header checksum is calculated in hardware

  bitdata FrameType :: Bit
    = frame_type_802_3    as 0
    | frame_type_ethernet as 1

  bitdata IPPayloadType :: Bits 3
    = ip_payload_type_unknown as 0b000 -- Unknown or not processed
    | ip_payload_type_udp     as 0b001
    | ip_payload_type_tcp     as 0b010
    | ip_payload_type_icmp    as 0b011
|]
