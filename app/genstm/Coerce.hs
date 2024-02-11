{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Coerce where

import Data.Char (toUpper, isDigit)
import Data.Default.Class (def)
import Data.Maybe
import qualified Data.List as L
import Text.Regex.Posix
import Safe

import Data.SVD
import Data.Ivory.ISR (fillMissing)
import Data.Ivory.Pretty (replace)
import Data.STM32.Types
import Data.STM32.Drivers

import Utils

-- toplevel kindof
procPeriph p ver x = pure $ new { periphName = show p }
  where
    new = filterByPeriph p ver x

-- given a composed field like pllp0 pllp1 merge this into multiple Bits or specific type set by adjust function
-- mergeFields "PLLP" [0..1] id will drop pllp1 and grow pllp0, renaming it to "pllp"
mergeFields prefix ids adjust x = adjustFields merger x
  where
    merger x | prefix ++ (show $ minimum ids) == fieldName x = Just $ setFieldName prefix $ grow $ adjust x
    merger x | or (map ((==fieldName x).(prefix++).show) ids) = Nothing
    merger x | otherwise = Just x
    grow x = x { fieldBitWidth = length ids }

setFieldType rt x = x { fieldRegType = Just rt }
setFieldName n x = x { fieldName = n }
setFieldWidth n x = x { fieldBitWidth = n }
setDescription n x = x { fieldDescription = n }

matchesRe :: String -> String -> Bool
matchesRe x re = x =~ re

-- turn uart6 into uart
dropID x | periphName x `matchesRe` "[A-Za-z]+[0-9]" = x { periphName = init $ periphName x }
dropID x | otherwise = error $ "Cannot dropID from " ++ (show x)

-- apply fn to peripherals registers
adjustRegs fn x@Peripheral{..} = x { periphRegisters = map fn periphRegisters }

-- apply fn to peripherals registers fields
adjustFields fn x@Peripheral{..} = adjustRegs adj x
  where
    adj reg@Register{..} = reg { regFields = mapMaybe fn regFields }

-- apply fn to peripherals registers fields passing register name
adjustRegFields fn x@Peripheral{..} = adjustRegs adj x
  where
    adj reg@Register{..} = reg { regFields = mapMaybe (fn regName) regFields }

-- apply fn to peripherals registers fields
adjustFieldsByRegName targetRegName fn x@Peripheral{..} = adjustRegs adj x
  where
    adj reg@Register{..} | regName == targetRegName = reg { regFields = mapMaybe fn regFields }
    adj reg@Register{..} | otherwise = reg

filterByPeriph ADC _ = adjustADCRegs
filterByPeriph GPIO (Just 1) = renameGPIO . adjustGPIOF1Regs
filterByPeriph GPIO (Just 2) = renameGPIO . adjustGPIORegs
filterByPeriph CAN  _        = adjustCANRegs
filterByPeriph UART v =
    (
      if (v == Just 3)
      then adjustUARTV3Regs
      else id
    )
  . adjustUARTRegs
filterByPeriph USART v =
    renameUSART
  . (
      if (v == Just 3)
      then adjustUARTV3Regs
      else id
    )
  . adjustUARTRegs
filterByPeriph RCC  _ = adjustRCCRegs
filterByPeriph SPI  _ = adjustSPIRegs
filterByPeriph SYSCFG _ = renameDualSYSCFG
filterByPeriph EXTI   _ = adjustEXTI
filterByPeriph _ _ = id

checkPeriphRegsContinuity p new = do
  assert ("Register of " <> (tshow p) <> " is NOT continuous")
    $ and $ mapRegs continuityCheck new


-- Special driver/peripheral regs versioning treatment
--
-- UART needs UART1 & UARTv2 for old/new periph regs, has common driver
-- USART -> UART
-- SPI has common regs but DR is 8 bit long and original is renamed to DR16
-- CAN needs special treatment for filters, bitArrays and composed FIFOs

filterRegsByName f x = x { periphRegisters = filter (f . regName) $ periphRegisters x }

canMailboxRegs :: [String]
canMailboxRegs = [
   "TDT[0-2]?R"
 , "TDL[0-2]?R"
 , "TDH[0-2]?R"
 , "TI[0-2]?R"
 , "RI[0-1]?R"
 , "RF[0-1]?R"
 , "RDT[0-1]?R"
 , "RDL[0-1]?R"
 , "RDH[0-1]?R"
 ]

adjustCANRegs x = addFilters
                $ mergeArbId
                $ mergeBitArrayFields
                $ filterRegsByName (not . (=~ canFilters))
                $ dropCANPrefix
                $ x { periphRegisters = L.nubBy (\x y -> regName x == regName y) $ map merge $ periphRegisters x }
  where
    merge reg = foldl re reg canMailboxRegs
    re reg match | regName reg =~ match =
      reg { regName = replace "[0-1]?" "" $ replace "[0-2]?" "" match
          , regFields = map (dropSuffixID reg) $ regFields reg }
    re reg _ = reg

    -- merge CAN arbitration fields stid + exid in TIxR RIxR into one id field
    mergeArbId = adjustFields fixId
    fixId f | fieldName f == "STID" = Nothing
    fixId f | fieldName f == "EXID" = Just $ setFieldName "ID" $ setFieldWidth 29 f
    fixId f | otherwise = Just f

    dropSuffixID r f | regName r == "CAN_RF0R" && (isDigit $ last $ fieldName f) = f { fieldName = init $ fieldName f }
    dropSuffixID r f | otherwise = f

    -- filterRXTX =  filterRegsByName (\x -> not ("R" `L.isPrefixOf` x || "T" `L.isPrefixOf` x))
    dropCANPrefix = adjustRegs rename
    rename x | "CAN_" `L.isPrefixOf` (map toUpper $ regName x) = x { regName = drop 4 $ regName x }
    rename x | otherwise = x

    addFilters p = p { periphRegisters = periphRegisters p ++ [firx16, firx32] }

    mergeBitArrayFields = mergeFields "FBM" [0..27] (setFieldType "BitArray 28 Bit")
                        . mergeFields "FSC" [0..27] (setFieldType "BitArray 28 Bit")
                        . mergeFields "FFA" [0..27] (setFieldType "BitArray 28 Bit")
                        . mergeFields "FACT" [0..27] (setFieldType "BitArray 28 Bit")


-- old attempt at generating parts of Peripheral.CAN
-- fix4PeripheralDefinition new = filterRegsByName (not . isDualReg) new
--   where
--     isDualReg x = or $ map (\match -> x =~ match) canDualRegs

-- bit arrays
canBitArrays = [ "FM1R", "FS1R", "FFA1R", "FA1R" ]

-- filters FiRx(32) FiRx(16)
-- F0R1 is Filter bank 0 register 1 up to F27Rx x in [1,2]
canFilters :: String
canFilters = "F[0-9][0-9]?R[1-2]"

makeReg name fields = let
  r = def
        { regName = name
        , regDisplayName = name
        , regSize = 32
        , regAccess = ReadWrite
        , regFields = mkFields fields
        }
  in (if continuityCheck r then r else error "Continuity check failed for mkReg")
  where
    mkFields xs = setOffsets 32 $ map mkField xs
    mkField (name, width) =
      def
        { fieldName = name
        , fieldBitWidth = width
        , fieldReserved = (name == "reserved")
        }
    setOffsets offs (x:xs) = (x { fieldBitOffset = offs - fieldBitWidth x }):(setOffsets (offs - fieldBitWidth x) xs)
    setOffsets 0 [] = []
    setOffsets _ _ = error "Offsets does not compute"

-- filter these but
-- add FiRx manually
firx16 = makeReg "FiRx16" [
    ("stid1", 11)
  , ("rtr1",  1)
  , ("ide1",  1)
  , ("exid1", 3)
  , ("stid0", 11)
  , ("rtr0",  1)
  , ("ide0",  1)
  , ("exid0", 3)
  ]

firx32 = makeReg "FiRx32" [
    ("stid", 11)
  , ("exid", 18)
  , ("ide",  1)
  , ("rtr",  1)
  , ("reserved",  1)
  ]


-- ADC
adjustADCRegs = adjustRegFields fix
  where
    fix "CR1" x | fieldName x == "RES" = Just $ x { fieldRegType = Just "ADCResolution" }
    fix _ x = Just x

-- UART
adjustUARTRegs x = adjustRegFields fix x
  where
    -- we fix these to 8 bits, they are defined as 9 bit iirc but that fucks with our drivers
    fix "DR" x | fieldName x == "DR" = Just $ x { fieldBitWidth = 8 }
    fix "DR" x | fieldReserved x = Just $ x { fieldBitWidth = 24 }

    fix "RDR" x | fieldName x == "RDR" = Just $ x { fieldBitWidth = 8 }
    fix "RDR" x | fieldReserved x = Just $ x { fieldBitWidth = 24 }

    fix "TDR" x | fieldName x == "TDR" = Just $ x { fieldBitWidth = 8 }
    fix "TDR" x | fieldReserved x = Just $ x { fieldBitWidth = 24 }

    -- mantissa is dropped and div bellow covers both mantissa and fraction
    fix "BRR" x | fieldName x == "DIV_Mantissa" = Nothing
    fix "BRR" x | fieldName x == "DIV_Fraction" =
      Just
        $ x
            { fieldBitWidth = 16
            , fieldName = "div"
            , fieldDescription = "divider"
            }
    fix "BRR" x | fieldReserved x = Just $ x { fieldBitWidth = 16 }

    -- types
    fix "CR1" x | fieldDescription x == "Word length" = Just $ x { fieldRegType = Just "UART_WordLen" }
    fix "CR2" x | fieldDescription x == "STOP bits" = Just $ x { fieldRegType = Just "UART_StopBits" }
    fix _ x = Just x

adjustUARTV3Regs x = adjustRegFields fix x
  where
    -- V3 UART only - normally this is 16 bits wide but LPUART uses 20
    fix "BRR" x | fieldName x == "BRR" =
      Just
        $ x
            { fieldBitWidth = 20
            , fieldName = "brr"
            , fieldDescription = "brr divider"
            }
    fix "BRR" x | fieldReserved x = Just $ x { fieldBitWidth = 12 }
    fix _ x = Just x

adjustGPIORegs x = adjustRegs rename $ adjustFields fix x
  where
    fix x | "PUPDR"   `L.isPrefixOf` (fieldName x) = Just $ setFieldType "GPIO_PUPD" x
    fix x | "AFR"     `L.isPrefixOf` (fieldName x) = Just $ setFieldType "GPIO_AF" x
    fix x | "OSPEEDR" `L.isPrefixOf` (fieldName x) = Just $ setFieldType "GPIO_Speed" x
    fix x | "OT"      `L.isPrefixOf` (fieldName x) = Just $ setFieldType "GPIO_OutputType" x -- gpio_ot vs gpio_otype_x
    fix x | "MODE"    `L.isPrefixOf` (fieldName x) = Just $ setFieldType "GPIO_Mode" x
    fix x = Just x
    rename x | regName x == "GPIOB_OSPEEDR" = x { regName = "OSPEEDR" }
    rename x | otherwise = x

adjustGPIOF1Regs x = adjustFields fix x
  where
    fix x | "MODE"    `L.isPrefixOf` (fieldName x) = Just $ setFieldType "GPIOF1_Mode" x
    fix x = Just x

renameGPIO x = x { periphName = fix $ periphName x }
  where
    -- drop 5 is not an error here, we're renaming GPIO[A-Z]
    fix x | "GPIO" `L.isPrefixOf` x = ("GPIO"++) . drop 5 $ x
    fix x | "gpio" `L.isPrefixOf` x = ("gpio"++) . drop 5 $ x
    fix x | otherwise = x

renameUSART x = fixRegs $ x { periphName = fix $ periphName x }
  where
    fix x | "USART" `L.isPrefixOf` x = ("UART"++) . drop 5 $ x
    fix x | "usart" `L.isPrefixOf` x = ("uart"++) . drop 5 $ x
    fix x | otherwise = x
    fixRegs = adjustRegs rename
    rename x = x { regName = fix $ regName x }

adjustRCCRegs x = cier2cir $ merges $ adjustFields fix x
  where
    fix x | "PPRE"    `L.isPrefixOf` (fieldName x)      = Just $ setFieldType "RCC_PPREx" x
    fix x | "HPRE"    `L.isPrefixOf` (fieldName x)      = Just $ setFieldType "RCC_HPRE" x
    fix x | "IOP"     `L.isPrefixOf` (fieldName x)      = Just $ x { fieldName = "GPIO" ++ (fromJust $ L.stripPrefix "IOP" $ fieldName x) }
    fix x | fieldName x `matchesRe` "MCO[0-9]?PRE"      = Just $ setFieldType "RCC_MCOxPre" x
    fix x | fieldName x `matchesRe` "MCO[0-9]?"
            && fieldBitWidth x == 2                     = Just $ setFieldType "RCC_MCOx" x
    fix x | fieldName x == "SW" || fieldName x == "SWS" = Just $ setFieldType "RCC_SYSCLK" x
    -- sometimes we get PLLP0 PLLP1 and sometimes PLLP with size 2..
    fix x | fieldName x == "PLLP" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLP" x
    fix x | otherwise                                   = Just x
    merges x = mergeFields "PLLP" [0..1] (setFieldType "RCC_PLLP")
             . mergeFields "PLLQ" [0..3] id
             . mergeFields "PLLN" [0..8] id
             . mergeFields "PLLM" [0..5] id
             . mergeFields "SW"   [0..1] (setFieldType "RCC_SYSCLK")
             . mergeFields "SWS"  [0..1] (setFieldType "RCC_SYSCLK")
             $ x
    cier2cir = adjustRegs fixCier
    fixCier r | regName r == "CIER" = r { regName = "CIR" }
    fixCier r = r

adjustSPIRegs p = addDR16 . (adjustRegs makeDR8Bit) . adjustFields fix $ p
  where fix x | fieldName x == "DR"                         = Just $ setFieldWidth 8 x
        fix x | fieldName x == "BR"                         = Just $ setFieldType "SPIBaud" x
        fix x | otherwise                                   = Just x
        addDR16 x = x { periphRegisters = periphRegisters x ++ [dr16Reg] }
        getDR = headNote "adjustSPIRegs getDR" . filter ((== "DR") . regName) $ periphRegisters p
        dr16Reg = getDR { regName = "DR16" , regDescription = "DR register with 16 bit DR field" }
        makeDR8Bit reg | regName reg == "DR" =
          reg
            { regSize = 8
            , regFields = dropReserved (regFields reg)
            }
        makeDR8Bit reg | otherwise = reg

        dropReserved :: [Field] -> [Field]
        dropReserved = filter (not . fieldReserved)

renameDualSYSCFG x = adjustRegs fix x
  where fix x | "SYSCFG_" `L.isPrefixOf` (regName x) = x { regName = fromJust $ L.stripPrefix "SYSCFG_" $ regName x }
        fix x | otherwise = x


adjustEXTI = adjustRegs make32bit
  where
    dataField = def
                  { fieldName = "data"
                  , fieldDescription = "Data"
                  , fieldBitWidth = 32
                  }
    make32bit reg = reg { regFields = [ dataField ] }
    --addEXTICR x@Peripheral{..} = x {
    --  periphRegisters = periphRegisters ++ [ makeReg "EXTICR" [ ("data", 32) ] ] }

usartToUart x | periphName x == "USART" = x { periphName = show UART }
usartToUart x | otherwise = x

adjustPeriphFamily l4s x | l4s == L4 || l4s == L4Plus = adjustFields fix x
  where
    fix x | fieldName x == "SW" || fieldName x == "SWS" = Just $ setFieldType "RCC_SYSCLK_L4" x
    fix x | fieldName x == "PLLQ" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLQR_L4" x
    fix x | fieldName x == "PLLR" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLQR_L4" x
    fix x | fieldName x == "PLLSRC" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLSRC_L4" x
    fix x | fieldName x == "MSIRANGE" && fieldBitWidth x == 4 = Just $ setFieldType "RCC_MSIRANGE_L4" x
    fix x = Just x
adjustPeriphFamily F0 x = adjustFields fix $ x
  where
    fix x | fieldName x == "SW" || fieldName x == "SWS" = Just $ setFieldType "RCC_SYSCLK_F0" x
    fix x | fieldName x == "PLLSRC" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLSRC_F0" x
    fix x = Just x
adjustPeriphFamily G0 x = adjustFields fix $ renamePLLSYS x
  where
    fix x | fieldName x == "SW" || fieldName x == "SWS" = Just $ setFieldType "RCC_SYSCLK_G0" x
    fix x | fieldName x == "PLLSRC" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLSRC_G0" x
    fix x = Just x
adjustPeriphFamily G4 x = adjustFields fix $ renamePLLSYS x
  where
    fix x | fieldName x == "SW" || fieldName x == "SWS" = Just $ setFieldType "RCC_SYSCLK_G4" x
    fix x | fieldName x == "PLLR" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLQR_G4" x
    fix x | fieldName x == "PLLQ" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLQR_G4" x
    fix x | fieldName x == "PLLSRC" && fieldBitWidth x == 2 = Just $ setFieldType "RCC_PLLSRC_G4" x
    fix x = Just x
adjustPeriphFamily _ x = x

renamePLLSYS = adjustRegs renameReg . adjustFields renameFields
  where
    renameReg x | regName x == "PLLSYSCFGR" = x { regName = "PLLCFGR" }
    renameReg x | otherwise = x

    renameFields x | "PLLSYS" `L.isPrefixOf` fieldName x =
      Just $ setFieldName ("PLL" ++ (fromJust $ L.stripPrefix "PLLSYS" $ fieldName x)) x
    renameFields x | otherwise = Just x


fixSVDs svds = map (fixLPUART . fixIncompletes) svds
  where
    -- incomplete interrupts maps
    -- F401, F41x, L4x5
    fixIncompletes (name, dev) | name == "F401" = (name, isrFilled)
    fixIncompletes (name, dev) | "F41" `L.isPrefixOf` name = (name, isrFilledF41X dev)
    fixIncompletes (name, dev) | name == "L4x5" = (name, isrFilledL4X5 dev)
    fixIncompletes x | otherwise = x

    -- fill missing of F401 with F405
    f401 = single svds "F401"
    f405 = single svds "F405"
    -- fill missing of F41x with F413
    f413 = single svds "F413"
    -- fill missing of L4x5 with L462
    l462 = single svds "L4x2"

    single svds what = snd . headNote "fixSVDs single" . filter (\(name, _) -> name == what) $ svds
    isrFilled = fillMissing f401 f405
    isrFilledF41X dev = fillMissing dev f413
    isrFilledL4X5 dev = fillMissing dev l462

    -- G031 got name LPUART vs correct LPUART1
    fixLPUART (name, dev) | "STM32G0" `L.isPrefixOf` deviceName dev = (name, dev { devicePeripherals = map (renameLPUART) $ devicePeripherals dev })
      where renameLPUART p | periphName p == "LPUART" = p { periphName = "LPUART1" }
            renameLPUART p | otherwise  = p
    fixLPUART (name, dev) | "STM32G4" `L.isPrefixOf` deviceName dev = (name, dev { devicePeripherals = map (renameLPUARTInterrupt) $ devicePeripherals dev })
      where renameLPUARTInterrupt p | periphName p == "LPUART1" = p { periphInterrupts = map reISR $ periphInterrupts p }
            renameLPUARTInterrupt p | otherwise  = p
            reISR i | interruptName i == "LPUART" = i { interruptName = "LPUART1" }
            reISR i | otherwise = i
    fixLPUART x | otherwise = x
