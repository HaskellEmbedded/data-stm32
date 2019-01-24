{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- RegTypes.hs --- Types for register fields in GPIOF1 pin driver.
--

module @modns@ where

import Ivory.Language

[ivory|
 bitdata GPIOF1_Mode :: Bits 2
  = gpio_mode_input        as 0
  | gpio_mode_output_10mhz as 1
  | gpio_mode_output_2mhz  as 2
  | gpio_mode_output_50mhz as 3

 bitdata GPIOF1_InputConfig :: Bits 2
  = gpio_input_conf_analog       as 0  -- Analog mode
  | gpio_input_conf_float        as 1  -- Floating input (reset state)
  | gpio_input_conf_pull_updown  as 2  -- Input with pull-up / pull-down
  -- | reserved  as 3

 bitdata GPIOF1_OutputConfig :: Bits 2
  = gpio_output_conf_pushpull     as 0
  | gpio_output_conf_opendrain    as 1
  | gpio_output_conf_af_pushpull  as 2
  | gpio_output_conf_af_opendrain as 3
|]
