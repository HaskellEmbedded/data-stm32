-- | MIIM Request response structs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.BSP.STM32.Driver.ETH.MIIMTypes where

import Ivory.Language
import Ivory.BSP.STM32.Peripheral.ETH.Phy (PhyRegister)

[ivory|
  struct miim_request
    { miim_request_address  :: Stored Uint8
    ; miim_request_register :: Stored PhyRegister
    ; miim_request_write    :: Stored IBool
    ; miim_request_data     :: Stored Uint16
    }

  struct miim_result
    { miim_result_okay :: Stored IBool
    ; miim_result_data :: Stored Uint16
    }
|]
