{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module MakePeriph where

import Control.Monad.Reader

import Debug.Trace
import Data.Maybe
import Data.Either (rights)
import Data.Ord (comparing)
import Data.Char (toLower, toUpper, isDigit)
import Data.Data (Data, Typeable)
import qualified Data.List as L
import qualified Data.Set as S

import Data.CMX
import Data.SVD
import Data.Ivory.ISR
import Data.STM32.Types
import Data.STM32.Drivers

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8

import Types
import Utils

data InstancesCtx = InstancesCtx {
    dev       :: String
  , fam       :: String
  , vers      :: String
  , instances :: [InstanceCtx]
  } deriving (Show, Data, Typeable)

data InstanceCtx = InstanceCtx {
    name         :: String
  , version      :: String
  , interrupts   :: [String]
  , clockSource  :: String
  , rccEnableReg :: String
  , rccEnableBit :: String
  , rccResetReg  :: String
  , rccResetBit  :: String
  , index        :: String
  , numericIndex :: Int
  } deriving (Show, Data, Typeable)



-- find enable and reset bits in RCC registers (e.g. uart{vers}{suffix} (e.g. uart1en))
findRCCBit :: Periph
           -> String
           -> String
           -> Peripheral
           -> (String, Field)
findRCCBit periph vers suffix rcc = get (rccFieldName suffix) $ periphRegisters rcc
  where
    rccFieldName prefix = map toUpper $ concat [show periph, vers, prefix]
    get name regs = case filterRegFieldsByName name regs of
         []         -> error $ "RCC field not found: " ++ name
         [(r, [f])] -> (regName r, f)
         [(r, _)]   -> error $ "Multiple fields found in RCC registers: " ++ name
         _          -> error $ "Field found in multiple RCC registers: " ++ name

filterRegFields :: (Field -> Bool)
                -> [Register]
                -> [(Register, [Field])]
filterRegFields fn regs = filter (\(r, fs) -> fs /= [])
        $ map (\r -> (r, filter fn $ regFields r)) regs

filterRegFieldsByName :: String
                      -> [Register]
                      -> [(Register, [Field])]
filterRegFieldsByName name regs = filterRegFields ((==name) .fieldName) regs

-- usart1 - 1
-- gpioa - a
periphParser :: Periph -> Parser Char
periphParser GPIO = string (B.pack $ show GPIO) *> letter_ascii <* endOfInput
periphParser p = string (B.pack $ show p) *> digit <* endOfInput

-- peripheral instances according to SVD
periphInstancesSVD :: Periph
                   -> MCU
                   -> MonadGen [Char]
periphInstancesSVD p mcu = do
  mm <- memMap mcu
  return $ L.sort
         $ rights
         $ map (parseOnly (periphParser p) . B.pack . snd) mm

-- peripheral instances according to IPs
periphInstancesIP :: Periph
                  -> MCU
                  -> MonadGen [Char]
periphInstancesIP p mcu = do
  let ips = mcuIps mcu
  return $ L.sort
         $ rights
         $ map (parseOnly (periphParser p) . B.pack . ipInstanceName) $ S.toList ips

-- similar to periphInstancesSVD but uses RCC register to
-- get a list of instances
periphInstancesRCC :: Periph
                -> MCU
                -> MonadGen [Char]
periphInstancesRCC periph mcu = do
  rcc <- processedPeriph RCC mcu
  return
    $ L.sort
    $ L.nub
    $ map (\Field{..} -> L.head $ fromJust $ L.stripPrefix pName $ fieldName)
    $ concatMap (\(r, fs) -> fs)
    $ filterRegFields (\Field{..} ->
           pName `L.isPrefixOf` fieldName
        && "EN" `L.isSuffixOf` fieldName
        && (not $ "LPEN" `L.isSuffixOf` fieldName))
        (periphRegisters rcc)
  where
    pName = show periph

periphInstancesData :: Periph -> MCU -> MonadGen [InstanceCtx]
periphInstancesData periph mcu = do
  dev <- svdForMCU mcu
  rcc <- processedPeriph RCC mcu

  -- get instances by svd
  is' <- periphInstancesSVD periph mcu
  -- and instances according to the RCC register
  rccis <- periphInstancesRCC periph mcu

  -- and instances according to the IPs
  -- unless this is GPIO we intersect with IPs as well
  ipis <- periphInstancesIP periph mcu
  let is = if (periph == GPIO) then is'
                               else is' `L.intersect` ipis

  case (is `L.intersect` rccis ) of
    [] -> return $ [mkData dev rcc '0' 0]
    xs -> return $
      filter (filterMissingInterrupt periph) $
      map (uncurry (mkData dev rcc)) (zip xs [0..])
  where
    pName idStr = concat [show periph, idStr]
    rccEn idStr rcc = findRCCBit periph idStr "EN" rcc
    rccRst idStr rcc = findRCCBit periph idStr "RST" rcc
    lower = map toLower
    mkData dev rcc idChar idx = InstanceCtx {
        name         = lower $ pName id'
      , version      = maybe "" show $ diVersion $ fromJust $ mcuPeriphDriver mcu periph
      , interrupts   = sharedInterrupts $ isrs' id' dev
      , clockSource  = maybe "" id $ pclkIndex $ fst $ rccEn id' rcc
      , rccEnableReg = lower $ fst $ rccEn id' rcc
      , rccEnableBit = lower $ fieldName $ snd $ rccEn id' rcc
      , rccResetReg  = lower $ fst $ rccRst id' rcc
      , rccResetBit  = lower $ fieldName $ snd $ rccRst id' rcc
      , index = id'
      , numericIndex = idx
      }
      where id' = if idChar == '0' then "" else [idChar]

    isrs' idStr dev = map interruptName
          $ L.sortBy (comparing interruptValue)
          $ isrsForPeriph idStr
          $ normalizeISRNames
          $ renameDups
          $ isrs [dev]

    sharedInterrupts is = case periph of
        I2C -> case is of
          -- we duplicate this interrupt as it is shared
          -- instead of two separate for status/error (F0 family)
          -- not tested if it actually works
          -- v2 driver is actually quite restricted on which flags
          -- each interrupt handler handles
          [x] -> [x, x]
          good@[_, _] -> good
          otherwise -> fail "Wrong number of interrupts for I2C periph"
        CAN -> case is of
          -- F0 uses only global CAN interrupt called CEC_CAN
          -- again not sure if driver can handle this
          [x] -> [x, x, x, x]
          good@[_, _, _, _] -> good
          otherwise -> fail "Wrong number of interrupts for CAN periph"
        _ -> is

    isrsForPeriph idStr = filter (\Interrupt{..} ->
      case periph of
        CAN -> case mcuFamily mcu of
          F0 -> "CEC_CAN" == interruptName
          _  ->    (pName idStr) `L.isPrefixOf` interruptName
                || (pName idStr) `L.isInfixOf` interruptName


        SPI ->    (pName idStr) `L.isInfixOf` interruptName
               && (not ("OCTOSPI" `L.isInfixOf` interruptName))

        _ ->    (pName idStr) `L.isPrefixOf` interruptName
             || (pName idStr) `L.isInfixOf` interruptName

      )

    filterMissingInterrupt periph ctx = log $ length (interrupts ctx) == validISRCount periph
      where
        log True = True
        log False = trace ("Missing interrupts for " ++ (name ctx) ++ " dev " ++ (mcuRefName mcu)) False

validISRCount UART = 1
validISRCount USART = 1
validISRCount SPI = 1
validISRCount I2C = 2
validISRCount RNG = 1
validISRCount CAN = 4
validISRCount _ = 0

makePeriphContext :: Periph -> MCU -> MonadGen InstancesCtx
makePeriphContext periph mcu = do
  inst <- periphInstancesData periph mcu

  return $ InstancesCtx {
    dev = devName
  , fam = show $ mcuFamily mcu
  , vers = vers inst
  , instances = inst
  }
  where
    devName = L.take 4 $ L.drop 5 $ mcuRefName mcu
    vers inst = case inst of
      (x:xs) -> version x
      [] -> ""

-- e.g. for G0s there's only one peripheral bus so we threat is as #1
pclkIndex name | "APB" `L.isPrefixOf` name = Just "PClk1"
pclkIndex name | "APB1" `L.isPrefixOf` name = Just "PClk1"
pclkIndex name | "APB2" `L.isPrefixOf` name = Just "PClk2"
pclkIndex name | "APB3" `L.isPrefixOf` name = Just "PClk3"
pclkIndex name | otherwise = Nothing
