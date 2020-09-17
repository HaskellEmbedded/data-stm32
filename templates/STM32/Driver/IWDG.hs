{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module {{ modns }}
  ( iwdgTower
  , iwdgTowerManual
  , IWDG()
  ) where

import Control.Monad (when)
import Data.List (findIndex)

import Ivory.Language
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Peripheral.IWDG.Peripheral
import Ivory.BSP.STM32.Peripheral.IWDG.Regs

iwdgTower :: (Time t)
          => IWDG
          -> t -- watchdog timeout
          -> Tower e ()
iwdgTower i t = do
  per <- period t
  trig <- iwdgTowerManual i t

  monitor "iwdgPeriod" $ do
    handler per "iwdgForward" $ do
      e <- emitter trig 1
      callback $ emit e


iwdgTowerManual :: (Time t)
                => IWDG
                -> t -- watchdog timeout
                -> Tower e (ChanInput ('Stored ITime))
iwdgTowerManual IWDG{..} t = do

  -- 1/(lsi / prescaler) * RL = timeout [s]
  -- for example
  -- 1/(32khz / 256) * 0xAAA = 21.84s
  --
  -- We compute allowed (prescaler, reload) values here
  -- according to this formula
  --
  let clkSrc = clockSourceHz iwdgClock
      pms = toMilliseconds t + 100 -- we add 100ms here to give period some time to react
      psc = [4, 8, 16, 32, 64, 128, 256]
      rls = [ (presc, rl)
        | presc <- psc
        , let rl = pms * clkSrc `div` (presc * 1000)
        , rl <= 0xFFF
        ]

  pokeChan <- channel

  monitor "iwdg" $ do
    monitorModuleDef $ do
      hw_moduledef

    handler systemInit "iwdgInit" $ do
      callback $ const $ do
        when (null rls) $ error $ "Unable to find IWDG timings for " ++ show pms ++ "ms with LSI " ++ show clkSrc
        let (pscVal, rl) = minimum rls
            pscIndex = case findIndex (==pscVal) psc of
              Just idx -> idx
              Nothing -> error "Can't happen"

        comment "Unlock IWDG"
        modifyReg iwdgRegKR $ do
          setField iwdg_kr_key (fromRep 0x5555)

        comment "Set prescaler"
        modifyReg iwdgRegPR $ do
          setField iwdg_pr_pr (fromRep $ fromIntegral pscIndex)

        comment "Set reload value"
        modifyReg iwdgRegRLR $ do
          setField iwdg_rlr_rl (fromRep $ fromIntegral rl)

        comment "Reload value"
        modifyReg iwdgRegKR $ do
          setField iwdg_kr_key (fromRep 0xAAAA)

        comment "Enable IWDG"
        modifyReg iwdgRegKR $ do
          setField iwdg_kr_key (fromRep 0xCCCC)

    handler (snd pokeChan) "iwdgUpdate" $ do
      callback $ const $ do
        comment "Update watchdog"
        modifyReg iwdgRegKR $ do
          setField iwdg_kr_key (fromRep 0xAAAA)

  return (fst pokeChan)
