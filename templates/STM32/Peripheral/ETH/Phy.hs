-- | Ethernet PHY registers
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.BSP.STM32.Peripheral.ETH.Phy where

import Ivory.Language

[ivory|
  -- Based on LAN8742A
  bitdata PhyRegister :: Bits 4
    = phy_register_bcr      as  0
    | phy_register_bsr      as  1
    | phy_register_phys_id1 as  2
    | phy_register_phys_id2 as  3
    | phy_register_ssr      as 31

  -- Basic control register
  bitdata PHY_BCR :: Bits 16 = phy_bcr
    { phy_bcr_reset             :: Bit -- Soft-reset phy
    , phy_bcr_loopback          :: Bit -- Enable loopback
    , phy_bcr_speed_sel_100mbit :: Bit -- Select 100Mbit speed
    , phy_bcr_autoneg_enable    :: Bit -- Enable auto negotiation
    , phy_bcr_power_down        :: Bit -- Enable power down mode
    , phy_bcr_isolate           :: Bit -- Enable isolation mode
    , phy_bcr_autoneg_restart   :: Bit -- Restart auto negotiation
    , phy_bcr_duplex_mode_full  :: Bit -- Enable full duplex mode
    , _                         :: Bits 8 -- (Reserved)
    }

  -- Basic status register
  bitdata PHY_BSR :: Bits 16 = phy_bsr
    { phy_bsr_100baseT4        :: Bit -- 100Base-T4 capable
    , phy_bsr_100baseTXFD      :: Bit -- 100Base-TX full-duplex capable
    , phy_bsr_100baseTXHD      :: Bit -- 100Base-TX half-duplex capable
    , phy_bsr_100baseTFD       :: Bit -- 10Base-T full-duplex capable
    , phy_bsr_100baseTHD       :: Bit -- 10Base-T half-duplex capable
    , phy_bsr_100baseT2FD      :: Bit -- 10Base-T2 full-duplex capable
    , phy_bsr_100baseT2HD      :: Bit -- 10Base-T2 half-duplex capable
    , phy_bsr_extended_status  :: Bit -- Extended status available
    , _                        :: Bit -- (Reserved)
    , _                        :: Bit -- (Reserved)
    , phy_bsr_autoneg_complete :: Bit -- Auto negotiation completed
    , phy_bsr_remote_fault     :: Bit -- Remote fault condition detected
    , phy_bsr_autoneg_capable  :: Bit -- Able to perform auto negotiation
    , phy_bsr_link_up          :: Bit -- Link status
    , phy_bsr_jabber_detected  :: Bit -- Jabber condition detected
    , phy_bsr_extended_caps    :: Bit -- Supports extended capabilities
    }

  -- Special status register
  bitdata PHY_SSR :: Bits 16 = phy_ssr
    { _                   :: Bits 3 -- (Reserved)
    , phy_ssr_autodone    :: Bit    -- Auto negotiation done
    , _                   :: Bits 7 -- (Reserved)
    , phy_ssr_full_duplex :: Bit    -- Full duplex negotiated
    , phy_ssr_100mbit     :: Bit    -- 100MBit negotiated
    , phy_ssr_10mbit      :: Bit    -- 10MBit negotiated
    , _                   :: Bits 2 -- (Reserved)
    }
|]
