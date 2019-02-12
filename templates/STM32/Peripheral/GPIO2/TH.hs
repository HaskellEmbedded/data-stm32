{-# LANGUAGE TemplateHaskell #-}
--
-- TH.hs --- Template Haskell macros for STM32F[2347] GPIO pins
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--

module @modns@ where

import Language.Haskell.TH

import Ivory.BSP.STM32.Peripheral.GPIO2.Peripheral

mkVar :: String -> Integer -> ExpQ
mkVar name n = do
  m <- lookupValueName (name ++ show n)
  case m of
    Just n' -> varE n'
    Nothing -> error $ "bad pin number or name, unable to lookup '" ++ (name ++ show n) ++ "'"

mkGPIOPin :: Name -> Name -> Integer -> [DecQ]
mkGPIOPin portName pin n =
  [ sigD pin [t| GPIOPin |]
  , valD (varP pin) (normalB (appsE ((conE 'GPIOPin) : fs))) []
  ]
  where fs = [ varE portName
             , litE (integerL n)
             , mkVar "gpio_mode_" n
             , mkVar "gpio_otype_" n
             , mkVar "gpio_ospeed_" n
             , mkVar "gpio_pupd_" n
             , mkVar "gpio_idr_" n
             , mkVar "gpio_bs_" n
             , mkVar "gpio_br_" n
             , if n < 8
                 then appE (conE 'AFRL) (mkVar "gpio_afrl_" n)
                 else appE (conE 'AFRH) (mkVar "gpio_afrh_" n)
             ]

-- | Define 16 GPIO pins for a GPIO port.
mkGPIOPins :: Name -> String -> Q [Dec]
mkGPIOPins portName baseName = sequence $ concat $ decls
  where decls = zipWith (mkGPIOPin portName) names nums
        nums  = [0..15]
        names = map (mkName . ((baseName ++) . show)) nums
