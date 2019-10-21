 module {{ modns }} where

import Ivory.Language (fromRep)
import Ivory.BSP.STM32.Peripheral.GPIO

import Data.Char (toUpper)
import Data.List (isInfixOf, isSuffixOf, intersect)

type AltFunctionDB = [(String, [(Int, [(String, Int)])])]

getPortPins :: String -> AltFunctionDB -> [(Int, [(String, Int)])]
getPortPins port db = case filter ((`isSuffixOf` port) . fst) db of
  [] -> error $ "No such port available: " ++ port
  [x] -> snd x
  _ -> error $ "Multiple ports found"

getPinAFs :: String -> Int -> AltFunctionDB -> [(String, Int)]
getPinAFs port pin db = case filter ((==pin) . fst) (getPortPins port db) of
  [] -> error $ "No such pin available: " ++ (show pin) ++ " on port " ++ port
  [x] -> snd x
  _ -> error $ "Multiple pins found"

getAFs :: String -> Int -> String -> AltFunctionDB -> [Int]
getAFs port pin periph db = case filter ((periph `isInfixOf`) . fst) (getPinAFs port pin db) of
  [] -> error $ "No alternate function found for peripheral: " ++ periph ++ " for port " ++ port ++ " and pin " ++ (show pin)
  xs -> map snd xs

findAFByPins' :: [GPIOPin] -> String -> AltFunctionDB -> [Int]
findAFByPins' pins periph db = foldr1 intersect $
  map (\pin -> if pinIsV1 pin
               then [0] -- for STM32F1s we don't have AFs but AFIO_REMAP, pinSetAF does nothing so we can use 0
               else getAFs (pinPort pin) (pinNumber pin) (map toUpper periph) db) pins

findAFByPins :: [GPIOPin] -> String -> AltFunctionDB -> GPIO_AF
findAFByPins pins periph db =  case findAFByPins' pins periph db of
  [x] -> fromRep $ fromIntegral x
  [] -> error $ "No alternate function found for peripheral: " ++ periph
  xs -> error $ "Multiple possible alternate functions found for peripheral: " ++ periph

