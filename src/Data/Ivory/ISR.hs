{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.ISR where

import Data.Ivory.Pretty

import Data.SVD
import Data.Char (toUpper)
import Data.List
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.Exit
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Data.Algorithm.Diff


-- take a list of devices from one family,
-- find device with most peripherals and
-- use it as representative for this family ISR map
--
-- use ppISRs to print these
isrs :: [Device] -> [Interrupt]
isrs devs = getDevISRs
          $ head
          $ reverse
          $ sortBy (comparing (length . devicePeripherals)) devs

getDevISRs :: Device -> [Interrupt]
getDevISRs Device{..} =
    sortBy (comparing (interruptValue))
  $ nubBy (\x y -> interruptValue x == interruptValue y)
  $ concatMap periphInterrupts devicePeripherals

diffISRs = getDiffBy (\a b -> interruptValue  a == interruptValue b && interruptName a == interruptName b)

-- 0xDEADCODE (allinone prototype)
-- ^^ eats STM32F4*.svd
genISRs fs = do
  is <- isr fs
  putStrLn $ (ppISRs . sortBy (comparing (interruptValue)) . nubBy (\x y -> interruptValue x == interruptValue y)) is

isr :: [String] -> IO ([Interrupt])
isr [] = return []
isr (f:fs) = do
  res <- parseSVD f
  case res of
    Left e -> do
      putStrLn ("No parse of " ++ f ++ " error was " ++ e)
      exitFailure
    Right x -> do
      is <- isr fs
      return $ is ++ (sortBy (comparing (interruptValue)) $ getDevISRs x)
