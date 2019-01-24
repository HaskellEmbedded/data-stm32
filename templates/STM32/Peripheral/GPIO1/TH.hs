{-# LANGUAGE TemplateHaskell #-}
--
-- TH.hs --- Template Haskell macros for STM32F1 GPIO pins
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module @modns@ where

import Language.Haskell.TH

import Ivory.BSP.STM32.Peripheral.GPIO1.Peripheral

mkVar :: String -> Integer -> ExpQ
mkVar name n = do
  m <- lookupValueName (name ++ show n)
  case m of
    Just n' -> varE n'
    Nothing -> error "bad pin number or name"

mkGPIOPin :: Name -> Name -> Integer -> [DecQ]
mkGPIOPin portName pin n =
  [ sigD pin [t| GPIOPin |]
  , valD (varP pin) (normalB (appsE ((conE 'GPIOPin) : fs))) []
  ]
  where fs = [ varE portName
             , litE (integerL n)
             , if n < 8
                 then appE (conE 'CRModeLow) (mkVar "gpio_crl_mode" n)
                 else appE (conE 'CRModeHigh) (mkVar "gpio_crh_mode" n)
             , if n < 8
                 then appE (conE 'CRConfLow) (mkVar "gpio_crl_cnf" n)
                 else appE (conE 'CRConfHigh) (mkVar "gpio_crh_cnf" n)
             , mkVar "gpio_odr_odr" n
             , mkVar "gpio_idr_idr" n
             , mkVar "gpio_bsrr_bs" n
             , mkVar "gpio_bsrr_br" n
             , mkVar "gpio_lckr_lck" n
             ]

-- | Define 16 GPIO pins for a GPIO port.
mkGPIOPins :: Name -> String -> Q [Dec]
mkGPIOPins portName baseName = sequence $ concat $ decls
  where decls = zipWith (mkGPIOPin portName) names nums
        nums  = [0..15]
        names = map (mkName . ((baseName ++) . show)) nums
