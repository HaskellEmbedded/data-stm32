-- | Phy initialization states
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ivory.BSP.STM32.Driver.ETH.PhyState where

import Ivory.Language

newtype PhyState = PhyState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

phyUnknown
  , phyResetReq
  , phyReset
  , phyResetClearedReq
  , phyResetCleared
  , phyPhysId1Req
  , phyPhysId1
  , phyPhysId2Req
  , phyPhysId2
  , phyLinkWaitReq
  , phyLinkWait -- 10
  , phyAutonegStartReq
  , phyAutonegStart
  , phyAutonegReq
  , phyAutoneg
  , phySsrReq
  , phySsr
  , phyDone
  :: PhyState

[   phyUnknown
  , phyResetReq
  , phyReset
  , phyResetClearedReq
  , phyResetCleared
  , phyPhysId1Req
  , phyPhysId1
  , phyPhysId2Req
  , phyPhysId2
  , phyLinkWaitReq
  , phyLinkWait
  , phyAutonegStartReq
  , phyAutonegStart
  , phyAutonegReq
  , phyAutoneg
  , phySsrReq
  , phySsr
  , phyDone
 ] = map (PhyState . fromInteger) [0..17]
