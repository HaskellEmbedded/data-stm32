{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module {{ modns }} where

import Ivory.HW
import Ivory.Language

import Ivory.BSP.STM32.Interrupt
import {{ init_modns }}.Regs

data {{ type }} = {{ type }}
{{ bitDataRegs }}
  , dacRCCEnable  :: forall eff . Ivory eff ()
  , dacRCCDisable :: forall eff . Ivory eff ()
  , dacInt        :: HasSTM32Interrupt
  , dacName       :: String
  }

-- | Create DAC given the base register address, global interrupt and its name
mk{{ type }}
  :: (STM32Interrupt i)
  => Integer -- ^ Base
  -> (forall eff . Ivory eff ()) -- ^ RCC Enable
  -> (forall eff . Ivory eff ()) -- ^ RCC Disable
  -> i -- ^ dac underrun interrupt
  -> String -- ^ Name
  -> {{ type }}
mk{{ type }} base rccen rccdis int n = {{ type }}
{{{ bitDataRegsMk }}}
  -- common status and control registers
  , dacRCCEnable  = rccen
  , dacRCCDisable = rccdis
  , dacInt        = HasSTM32Interrupt int
  , dacName       = n
  }
  where
  reg :: (IvoryIOReg (BitDataRep d)) => Integer -> String -> BitDataReg d
  reg offs name = mkBitDataRegNamed (base + offs) (n ++ "->" ++ name)

data DACChannel
  = DACChannel_1
  | DACChannel_2
  deriving (Eq, Enum, Ord, Show)

data DACMode
  = DACMode_8Bit_RightAligned
  | DACMode_12Bit_LeftAligned
  | DACMode_12Bit_RightAligned
  deriving (Eq, Enum, Ord, Show)

data DACBuffering
  = DACBuffering_Buffered -- ^ Default
  | DACBuffering_Unbuffered
  deriving (Eq, Enum, Ord, Show)

-- | Enable DAC peripheral
dacInit
  :: DAC
  -> Ivory eff ()
dacInit periph = do
  dacRCCEnable periph

-- | Enable a DAC channel
dacEnableChannel
  :: DAC
  -> DACChannel
  -> DACBuffering
  -> Ivory eff ()
dacEnableChannel periph chan buffering =
  modifyReg (dacRegCR periph)
    $ case chan of
        DACChannel_1 -> do
          case buffering of
            DACBuffering_Buffered ->
              clearBit dac_cr_boff1
            DACBuffering_Unbuffered ->
              setBit dac_cr_boff1

          setBit dac_cr_en1

        DACChannel_2 -> do
          case buffering of
            DACBuffering_Buffered ->
              clearBit dac_cr_boff2
            DACBuffering_Unbuffered ->
              setBit dac_cr_boff2

          setBit dac_cr_en2

-- | Write a new value to DAC channel
-- and poke its software trigger
dacWrite
  :: DAC
  -> DACChannel
  -> DACMode -- ^ Bit width and alignment
  -> Uint16
  -> Ivory eff ()
dacWrite periph chan mode val = do
  case chan of
    DACChannel_1 -> do
      case mode of
        DACMode_8Bit_RightAligned ->
          setReg (dacRegDHR8R1 periph)
            $ setField dac_dhr8r1_dacc1dhr
            $ fromRep
            $ bitCast val
        DACMode_12Bit_LeftAligned ->
          setReg (dacRegDHR12L1 periph)
            $ setField dac_dhr12l1_dacc1dhr
            $ fromRep
            $ bitCast val
        DACMode_12Bit_RightAligned ->
          setReg (dacRegDHR12R1 periph)
            $ setField dac_dhr12r1_dacc1dhr
            $ fromRep
            $ bitCast val

    DACChannel_2 -> do
      case mode of
        DACMode_8Bit_RightAligned ->
          setReg (dacRegDHR8R2 periph)
            $ setField dac_dhr8r2_dacc2dhr
            $ fromRep
            $ bitCast val
        DACMode_12Bit_LeftAligned ->
          setReg (dacRegDHR12L2 periph)
            $ setField dac_dhr12l2_dacc2dhr
            $ fromRep
            $ bitCast val
        DACMode_12Bit_RightAligned ->
          setReg (dacRegDHR12R2 periph)
            $ setField dac_dhr12r2_dacc2dhr
            $ fromRep
            $ bitCast val
