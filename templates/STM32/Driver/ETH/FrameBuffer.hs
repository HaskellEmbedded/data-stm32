-- | Ethernet frame buffer
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32.Driver.ETH.FrameBuffer where

import Ivory.Language

[ivory|
  -- Has to be aligned to 4 bytes
  -- 1522 comes from datasheet (1518 maximum legal length + 4 VLAN tag)
  string struct FrameBuffer 1522
|]
