{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Ivory.ISR where

import Data.Ivory.Pretty

import Data.SVD
import Data.Char (toUpper)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Ord (comparing)
import System.Exit
import Data.Algorithm.Diff
import Prettyprinter

import qualified Data.SVD.Util

-- take a list of devices from one family,
-- find device with most peripherals and
-- use it as representative for this family ISR map
--
-- use displayISRs to print these
isrs :: [Device] -> [Interrupt]
isrs devs = getDevISRs
          $ head
          $ reverse
          $ sortBy (comparing (length . devicePeripherals)) devs

-- fill `targets` missing interrupts from `source` device
-- (according to interrupt values)
--
-- e.g. F401 is missing some interrupts compatible with F405 (USART1..3)
-- we use `fillMissing f401 f405` to fix this
fillMissing :: Device -> Device -> Device
fillMissing target source = target { devicePeripherals = addToFirst $ devicePeripherals target }
  where
    targets = getDevISRs target
    targetsValues = map interruptValue srcs

    srcs = wwdg:(getDevISRs source)

    fills = filter (\x -> interruptValue x `elem` targetsValues) srcs

    wwdg = Interrupt {
      interruptName = "WWDG"
    , interruptValue = 0
    , interruptDescription = "Window Watchdog interrupt"
    }

    addToFirst (x:xs) = (x { periphInterrupts = periphInterrupts x ++ fills }):xs

getDevISRs :: Device -> [Interrupt]
getDevISRs Device{..} =
    sortBy (comparing (interruptValue))
  $ Data.SVD.Util.fillMissingInterrupts
  $ nubBy (\x y -> interruptValue x == interruptValue y)
  $ concatMap periphInterrupts devicePeripherals

diffISRs = getDiffBy (\a b -> interruptValue  a == interruptValue b && interruptName a == interruptName b)

-- 0xDEADCODE (allinone prototype)
-- ^^ eats STM32F4*.svd
genISRs fs = do
  is <- isr fs
  putStrLn $ (displayISRs . sortBy (comparing (interruptValue)) . nubBy (\x y -> interruptValue x == interruptValue y)) is

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

-- XXX: this should go to Coerce
normalizeISRNames xs = map (\x -> x { interruptName = norm (interruptName x) }) xs
  where norm name = replace "_IRQ" ""  -- for F0 we have WWDG_IRQ so normalize these
                  $ replace "lptim1_OR_it_eit_23" "LPTIM1_EXT1_23"
                    name

-- XXX breaks things when there are interrupt names ending with _
renameDups xs = reverse $ snd $ foldl f (S.empty, []) xs
  where f (seen, result) item | S.member (interruptName item) seen = f (seen, result) (item { interruptName = (interruptName item) ++ "_" })
                              | otherwise                          = (S.insert (interruptName item) seen, item:result)
