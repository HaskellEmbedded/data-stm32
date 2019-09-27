{-# LANGUAGE TemplateHaskell #-}
--
-- TH.hs --- Template Haskell macros for STM32F[2347] GPIO pins
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module Ivory.BSP.STM32.Peripheral.GPIOv2.TH where

import Language.Haskell.TH

import Ivory.BSP.STM32.Peripheral.GPIOv2.Peripheral
import qualified Ivory.BSP.STM32.Peripheral.GPIO as GPIOWrapper

mkVar :: String -> Integer -> ExpQ
mkVar name n = do
  m <- lookupValueName (name ++ show n)
  case m of
    Just n' -> varE n'
    Nothing -> error $ "bad pin number or name, unable to lookup '" ++ (name ++ show n) ++ "'"

mkGPIOPin :: Name -> Name -> Integer -> [DecQ]
mkGPIOPin portName pin n =
  [ sigD pin [t| GPIOWrapper.GPIOPin |]
  , valD (varP pin) (normalB (appE (conE 'GPIOWrapper.GPIOFX) (appsE ((conE 'GPIOPin) : fs)))) []
  ]
  where fs = [ varE portName
             , litE (integerL n)
             , mkVar "gpio_moder_moder" n
             , mkVar "gpio_otyper_ot" n
             , mkVar "gpio_ospeedr_ospeedr" n
             , mkVar "gpio_pupdr_pupdr" n
             , mkVar "gpio_idr_idr" n
             , mkVar "gpio_bsrr_bs" n
             , mkVar "gpio_bsrr_br" n
             , if n < 8
                 then appE (conE 'AFRL) (mkVar "gpio_afrl_afrl" n)
                 else appE (conE 'AFRH) (mkVar "gpio_afrh_afrh" n)
             ]

-- | Define 16 GPIO pins for a GPIO port.
mkGPIOPins :: Name -> String -> Q [Dec]
mkGPIOPins portName baseName = sequence $ concat $ decls
  where decls = zipWith (mkGPIOPin portName) names nums
        nums  = [0..15]
        names = map (mkName . ((baseName ++) . show)) nums
