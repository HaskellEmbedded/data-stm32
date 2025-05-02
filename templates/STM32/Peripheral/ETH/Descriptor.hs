{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.BSP.STM32.Peripheral.ETH.Descriptor where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.ETH.Descriptor.Types

[ivory|
  -- | Transmit
  --
  -- TDES0: Transmit descriptor Word0
  --   Owned, control and status fields
  bitdata ETH_TDES0 :: Bits 32 = eth_tdes0
    { eth_tdes0_own  :: Owned  -- Owned by CPU or DMA
    , eth_tdes0_ic   :: Bit    -- Interrupt on completion
    , eth_tdes0_ls   :: Bit    -- Last segment
    , eth_tdes0_fs   :: Bit    -- First segment
    , eth_tdes0_dc   :: Bit    -- Disable CRC
    , eth_tdes0_dp   :: Bit    -- Disable padding
    , eth_tdes0_ttse :: Bit    -- Transmit time stamp enable
    , _              :: Bit    -- (Reserved)
    , eth_tdes0_cic  :: CIC    -- Checksum insertion control
    , eth_tdes0_ter  :: Bit    -- Transmit end of ring (final descriptor reached)
    , eth_tdes0_tch  :: Bit    -- Transmit chained (when set treat second address as a next descriptor instead of a second data buffer)
    , _              :: Bit    -- (Reserved)
    , _              :: Bit    -- (Reserved)
    -- Status bits
    , eth_tdes0_ttss :: Bit    -- Transmit time stamp status
    , eth_tdes0_ihe  :: Bit    -- IP header error
    , eth_tdes0_es   :: Bit    -- Error summary (logical OR of other errors)
    , eth_tdes0_jt   :: Bit    -- Jabber timeout
    , eth_tdes0_ff   :: Bit    -- Frame flushed
    , eth_tdes0_ipe  :: Bit    -- IP payload error
    , eth_tdes0_lca  :: Bit    -- Loss of carrier
    , eth_tdes0_nc   :: Bit    -- No carrier
    , eth_tdes0_lco  :: Bit    -- Late collision
    , eth_tdes0_ec   :: Bit    -- Excessive collision
    , eth_tdes0_vf   :: Bit    -- VLAN frame
    , eth_tdes0_cc   :: Bits 4 -- Collision count
    , eth_tdes0_ed   :: Bit    -- Excessive deferral
    , eth_tdes0_uf   :: Bit    -- Underflow error
    , eth_tdes0_db   :: Bit    -- Deferred bit (half duplex only)
    }

   -- TDES1: Transmit descriptor Word1
   --   Buffer lengths
   bitdata ETH_TDES1 :: Bits 32 = eth_tdes1
     { _              :: Bits 3  -- (Reserved)
     , eth_tdes1_tbs2 :: Bits 13 -- Transmit buffer 2 size
     , _              :: Bits 3  -- (Reserved)
     , eth_tdes1_tbs1 :: Bits 13 -- Transmit buffer 1 size
     }

  -- TDES2: Address pointer
  --   to the first buffer of the descriptor
  -- TDES3: Address pointer
  --   to the second buffer of the descriptor or next descriptor
  -- TDES4: (Reserved)
  -- TDES5: (Reserved)
  -- TDES6: Time stamp low (PTP)
  -- TDES7: Time stamp high (PTP)

  -- | Receive
  --
  -- RDES0: Receive descriptor Word0
  --   Owned and status bits
  bitdata ETH_RDES0 :: Bits 32 = eth_rdes0
    { eth_rdes0_own   :: Owned     -- Owned by CPU or DMA
    , eth_rdes0_afm   :: Bit       -- Destination address filter fail
    , eth_rdes0_fl    :: Bits 14   -- Frame length (including CRC)
    , eth_rdes0_es    :: Bit       -- Error summary (logical OR of other errors)
    , eth_rdes0_de    :: Bit       -- Descriptor error
    , eth_rdes0_saf   :: Bit       -- Source address filter fail
    , eth_rdes0_le    :: Bit       -- Length error
    , eth_rdes0_oe    :: Bit       -- Overflow error
    , eth_rdes0_vlan  :: Bit       -- VLAN tagged frame
    , eth_rdes0_fs    :: Bit       -- First descriptor
    , eth_rdes0_ls    :: Bit       -- Last descriptor
    , eth_rdes0_iphce :: Bit       -- IP header checksum error / time stamp valid
    , eth_rdes0_lco   :: Bit       -- Late collision
    , eth_rdes0_ft    :: FrameType -- Frame type
    , eth_rdes0_rwt   :: Bit       -- Receive watchdog timeout
    , eth_rdes0_re    :: Bit       -- Receive error
    , eth_rdes0_drbe  :: Bit       -- Dribble bit error
    , eth_rdes0_ce    :: Bit       -- CRC error
    , eth_rdes0_esa   :: Bit       -- Extended status available in RDES4 / payload checksum error (when extended descriptors are not used)
    }

  -- RDES1: Receive descriptor Word1
  bitdata ETH_RDES1 :: Bits 32 = eth_rdes1
    { eth_rdes1_dic  :: Bit     -- Disable interrupt on completion
    , _              :: Bits 2  -- (Reserved)
    , eth_rdes1_rbs2 :: Bits 13 -- Receive buffer 2 size
    , eth_rdes1_rer  :: Bit     -- Receive end of ring
    , eth_rdes1_rch  :: Bit     -- Second address chained
    , _              :: Bit     -- (Reserved)
    , eth_rdes1_rbs1 :: Bits 13 -- Receive buffer 1 size
    }
  -- RDES2: Address pointer
  --   to the first buffer of the descriptor
  -- RDES3: Address pointer
  --   to the second buffer of the descriptor or next descriptor
  -- RDES4: Extended status
  bitdata ETH_RDES4 :: Bits 32 = eth_rdes4
    { _                :: Bits 18        -- (Reserved)
    , eth_rdes4_pv     :: Bit            -- PTP version
    , eth_rdes4_pft    :: Bit            -- PTP frame type
    , eth_rdes4_pmt    :: Bits 4         -- PTP message type
    , eth_rdes4_ipv6pr :: Bit            -- IPv6 packet received
    , eth_rdes4_ipv4pr :: Bit            -- IPv4 packet received
    , eth_rdes4_ipcb   :: Bit            -- IP checksum bypassed
    , eth_rdes4_ippe   :: Bit            -- IP payload error
    , eth_rdes4_iphe   :: Bit            -- IP header error
    , eth_rdes4_ippt   :: IPPayloadType  -- IP payload type if checksum offload is enabled
    }
  -- RDES5: (Reserved)
  -- RDES6: Time stamp low (PTP)
  -- RDES7: Time stamp high (PTP)
|]
