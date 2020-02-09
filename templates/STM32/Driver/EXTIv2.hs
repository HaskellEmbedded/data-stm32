{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module {{ modns }} (extiTower) where

import Control.Monad hiding (when)
import qualified Control.Monad
import Data.List (groupBy)

import Ivory.Language
import Ivory.Stdlib ((+=), when)
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Interrupt
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.EXTI.RegTypes
import Ivory.BSP.STM32.Peripheral.EXTI.Types
import Ivory.BSP.STM32.Peripheral.EXTIv2.Peripheral
import Ivory.BSP.STM32.Peripheral.EXTIv2.Regs

import qualified Ivory.BSP.ARMv7M.Instr as Instr

-- | Width of each field in extiCR${X} registers
extiCRFieldWidth :: Int
extiCRFieldWidth = 8

isrForPin :: EXTI -> GPIOPin -> HasSTM32Interrupt
isrForPin exti pin = case filter (\(s, e, _i) ->
     s <= pinNumber pin
  && e >= pinNumber pin) $ extiInterrupts exti of
    [] -> error $ "No EXTI interrupt for pin" ++ pinName pin
    [(_, _, x)] -> x
    _xs -> error $ "Multiple EXTI interrupts for pin" ++ pinName pin

extiTower :: EXTI
          -> [EXTIPin]
          -> Tower e [ChanOutput ('Stored IBool)]
extiTower exti ePins = do
  let
      -- pins groupped by their respective interrupts
      isrGroups = groupBy
        (\a b -> interruptIRQn (snd a) == interruptIRQn (snd b))
        $ map (\p -> (p, isrForPin exti $ extiPin p)) ePins

  -- groups with (channel, pin, interrupt)
  isrGroupsChan <- forM isrGroups $ \g -> do
    forM g $ \(epin, isr) -> do
      c <- channel
      return (c, epin, isr)

  -- groups with (channel, pin, interrupt, signalUnsafe channel)
  isrs <- forM isrGroupsChan $ \g -> forM g $ \(chan, epin, int) -> do
    sigChan <- signalUnsafe
              (Interrupt int)
              (Microseconds 250)
              (interrupt_disable int)
    return (chan, epin, int, sigChan)

  monitor "exti" $ do
    monitorModuleDef $ do
      hw_moduledef
      Instr.instrModuleDef

    forM_ isrs $ \g -> do
      -- create one handler for each group using common int/sigChan
      let (_, _, int, sigChan) = head g

      cnt <- stateInit "exti_cnt" (ival (0 :: Uint8))

      handler sigChan "extint_handle" $ do
        -- emitters for each pin, but callback is per isr, care
        pinEmitters <- forM g $ \(chan, epin, _, _) -> do
          pe <- emitter (fst chan) 1
          return (epin, pe)
        callback $ const $ do
          comment $ "Handler for " ++ show (interruptIRQn int)
          -- PR is split into RPR FPR
          -- rising and falling pending regs
          regValRising <- getReg (extiRegRPR1 exti)
          regValFalling <- getReg (extiRegFPR1 exti)

          when ((regValRising /=? fromRep 0) .|| (regValFalling /=? fromRep 0)) $ do
            forM_ pinEmitters $ \(p, e) -> do
              thisPinRising <- assign $ 1 ==? 0x1 .& (
                (toRep regValRising) `iShiftR`
                (fromIntegral $ pinNumber $ extiPin p))

              when thisPinRising $ do
                comment $ "Is pin rising " ++ (show $ extiPin p)
                modifyReg (extiRegRPR1 exti) $
                  setField exti_rpr1_data $ fromRep $
                    (fromIntegral (1 :: Int)) `iShiftL`
                    (fromIntegral $ pinNumber $ extiPin p)

              thisPinFalling <- assign $ 1 ==? 0x1 .& (
                (toRep regValFalling) `iShiftR`
                (fromIntegral $ pinNumber $ extiPin p))

              when thisPinFalling $ do
                comment $ "Is pin falling " ++ (show $ extiPin p)
                modifyReg (extiRegFPR1 exti) $
                  setField exti_fpr1_data $ fromRep $
                    (fromIntegral (1 :: Int)) `iShiftL`
                    (fromIntegral $ pinNumber $ extiPin p)

                cnt += 1
                emitV e true

            call_ Instr.dsb
            return ()

          interrupt_enable int

    handler systemInit "extiInit" $ do
      callback $ const $ do

        extiEnable exti

        forM_ isrGroups $ \g -> forM_ g $ \(epin, _int) -> do
          let p = extiPin epin
          pinEnable   p
          pinSetMode  p gpio_mode_input
          pinSetPull  p (extiPull epin)
          pinSetSpeed p gpio_speed_50mhz

          let m = pinNumber p `mod` 4
              reg = case pinNumber p `div` 4 of
                      0 -> extiCR1
                      1 -> extiCR2
                      2 -> extiCR3
                      3 -> extiCR4
                      _ -> error "Can't happen"

              -- value for 32bit registers
              pinVal = (fromIntegral (1 :: Int)) `iShiftL`
                         (fromIntegral $ pinNumber p)

          comment $ "Pin number " ++ (show $ pinNumber p)
          comment $ "Port number" ++ (show $ pinPortNumber p)
          comment $ "m" ++ (show $ pinNumber p `mod` 4)

          oldCR <- getReg (reg exti)
          modifyReg (reg exti) $ setField exti_exticr_data $
            fromRep $ (fromIntegral $ pinPortNumber p)
              `iShiftL` (fromIntegral $ extiCRFieldWidth * m)
              + (toRep $ oldCR #. exti_exticr_data)

          -- 0x0 is port A
          -- 0x1 is port B
          -- 0x2 is port C ^^
          --
          -- E.G. for PC13
          -- reg = 13 `div` 4 = 3 (+1 = EXTICR4)
          -- m = 13 `mod` 4 = 1
          -- shiftL (m * 4) as fields are 4 bit wide

          -- un-mask
          oldIMR <- getReg (extiRegIMR1 exti)
          modifyReg (extiRegIMR1 exti) $
            setField exti_imr1_data $ fromRep $ pinVal + (toRep $ oldIMR #. exti_imr1_data)

          -- edges
          Control.Monad.when (extiEdge epin `elem` [Rising, Both]) $ do
            oldRTSR <- getReg (extiRegRTSR1 exti)
            modifyReg (extiRegRTSR1 exti) $
              setField exti_rtsr1_data $ fromRep $ pinVal + (toRep $ oldRTSR #. exti_rtsr1_data)

          Control.Monad.when (extiEdge epin `elem` [Falling, Both]) $ do
            oldFTSR <- getReg (extiRegFTSR1 exti)
            modifyReg (extiRegFTSR1 exti) $
              setField exti_ftsr1_data $ fromRep $ pinVal + (toRep $ oldFTSR #. exti_ftsr1_data)

        --enable ISRs
        forM_ isrGroups $ \g -> do
          let (_pin, int) = head g

          interrupt_set_priority int 10
          interrupt_enable int

  return $ flip concatMap isrGroupsChan $ map (\(chan, _pin, _int) -> snd chan)
