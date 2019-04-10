{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import System.Exit
import Prelude hiding (FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import Data.Serialize
import qualified Data.ByteString.Char8 as B

import qualified Data.List as L
import qualified Data.Map as Map

import Text.Regex.Posix
import Text.Pretty.Simple

import Data.Maybe
import Data.Ord (comparing)

import Data.Ivory
import Data.Ivory.Pretty
import Data.Ivory.ISR
import Data.Ivory.MCU
import Data.Ivory.MemMap
import Data.Ivory.Periph

import Data.SVD hiding (svd, ppPeripheral)
--import Data.CMX

import Data.Serialize
import Debug.Trace

import Extract
import Utils

cdmk dir = do
  hasdir <- testdir dir
  when (not hasdir) (mktree dir)
  cd dir

supFamilies =
  [ F0
  , F1
  , F3
  , F4
  , F7
  , L4 ]

data Periph =
    ADC
  | AFIO -- F1 alternate function controls / remapping, proly good idea to turn it on
  | ATIM
  | GTIM
  | CAN
  | CEC
  | CRC
  | CRYP
  | DAC
  | DBG
  | DCMI
  | DMA
  | DMA2D
  | Ethernet
  | EXTI
  | FLASH
  | FPU
  | FSMC
  | GPIO
  | HASH
  | I2C
  | IWDG
  | LPTIM
  -- LTDC
  | MPU
  | NVIC
  -- PF
  | PWR
  | QUADSPI
  | RCC
  | RNG
  | RTC
  -- SAI
  -- SCB
  -- SDMMC
  -- SPDIF_RX
  | SPI
  -- STK
  | SYSCFG
  | UART
  | USB_OTG_FS
  | USB_OTG_HS
  | WWDG
  deriving (Show, Eq, Ord)

supPeriphs =
 [ CAN
 , UART
 , GPIO ]

periph2svdName :: Periph -> String
periph2svdName UART = "USART"
periph2svdName x = show x

-- crap
fixAndPack = T.replace "usart" "uart"
 . T.replace "USART" "UART"
 . T.pack

-- import qualified Ivory.BSP.STM32<fam>.<what> as <fam>
familyImport fam what = T.pack $ "import qualified Ivory.BSP.STM32" ++ (show fam) ++ "." ++ what ++ " as " ++ (show fam)

familyImports what = map (flip familyImport what) supFamilies


devFilter :: String -> Bool
devFilter x = x =~ ("STM32.*" :: String)
--devFilter x = x =~ ("STM32F.*" :: String)
--devFilter x = x =~ ("STM32F7.*5" :: String)

svdsFamily :: Family -> [(String, Device)] -> [Device]
svdsFamily f = map snd . filter (\(name, svd) -> (show f) `L.isPrefixOf` name)

isrsFamily f = isrs . (svdsFamily f)

main = do
  cd "data"
  dir <- pwd

  hasGarbage <- testdir "src"
  when hasGarbage $ do
    rmtree "src"
    rmtree "support"
    rm "ivory-bsp-stm32.cabal"

  mktree "src"
  mktree "support"

  svds <- svd devFilter

  -- svd names look like STM32F41x, we replace x with [0-9] so we can regex match on them
  -- prefer longer pattern
  let svdByMcu m = reverse
                 $ L.sortBy (comparing fst)
                 $ filter (\(name, dev) ->
                              (m =~ (replace "x" "[0-9]" $ take 4 name)) :: Bool) svds
      getSVD = snd . head . svdByMcu

  cd dir
  (shortMCUs, families, mcuxmls) <- cmx

  --for STM32XY/Periph/Regs
  --print $ mergedRegistersForPeriph "USART" svds

  cd dir
  stm32toplevel
  stm32periphs getSVD

  stm32families svds shortMCUs
  stm32devs getSVD

  -- cabal file and support files
  cd dir
  mods <- prefixRest . L.sort . map (T.replace ".hs" "" . T.replace "/" "." . T.replace "./src/" "" . fpToText)
            <$> fold (find (suffix ".hs") "./src/") Fold.list
  t <- procTemplate "ivory-bsp-stm32.cabal" [("exposed", T.intercalate ",\n" mods)]
  TIO.writeFile "ivory-bsp-stm32.cabal" t
  cptree "../templates/support/" "./support/"
  where
    prefixRest (x:xs) = x:(map (\y -> T.concat [T.replicate (T.length "exposed-modules:       ") " ", y]) xs)

thesePlease :: [String]
thesePlease =
  [ "F103"
  , "F427" ]

stm32devs get = do
  forM (map (\x -> (x, get $ "STM32" ++ x)) thesePlease) $ \(namePart, dev) -> do
    putStrLn $ "Processing device " ++ (show (deviceName dev))
    -- memMap
    (ns, t) <- procDevTemplate namePart "MemoryMap"
                [ ("dev", T.pack namePart )
                , ("map" , T.pack $ ppMemMap $ getDevMemMap dev ) ]

    writeHS ns t

    let genPeriph p = case filter ((==periph2svdName p) . periphGroupName) $ devicePeripherals dev of
         [] -> fail $ "No " ++ (show p) ++ " found"
         [x] -> do
               let new = procPeriph p Nothing x
               (ns, t) <- procDevTemplate namePart (T.pack . show $ p)
                 [ ("dev", T.pack namePart)
                 , ("regs", T.concat [ T.pack $ ppPeriphRegsWithDefs new]) ]

               writeHS ns t

    forM_ [ RCC, FLASH, PWR ] genPeriph

    forM_ supPeriphs $ \p -> do
      -- XXX: handle UART vs USART?
      case filter (((periph2svdName p)`L.isPrefixOf`) . periphName) $ devicePeripherals dev of
        [] -> fail $ "No peripheral found with groupName " ++ show p ++ " for device " ++ (deviceName dev)
        xs -> do
          print $ map periphName xs
          (ns, t) <- procDevTemplate namePart "UART"
            [ ("dev", T.pack namePart)
            , ("fam", T.take 2 $ T.pack namePart)
            , ("imports", "") ]
          writeHS ns t


-- generate peripheral definitions (src/Ivory/BSP/STM32/Peripheral/
-- for all supPeriphs
stm32periphs get = do
  -- base non-versioned peripherals on this devices svd
  let nonVersionedBase = "STM32F765"
      nVDev = get nonVersionedBase

  forM_ supPeriphs $ \p -> do
    case filter ((==periph2svdName p) . periphGroupName) $ devicePeripherals nVDev of
      [] -> fail $ "No peripheral found with groupName " ++ show p ++ " for device " ++ (deviceName nVDev)
      x:xs -> do

        case versioned p of
          False -> do
             -- register bitdata
            let new = procPeriph p Nothing x
                res = ppPeriphRegs new
            (ns, t) <- procPeriphTemplate (tshow p) "STM32.Peripheral.X.Regs" Nothing
              [ ("regs", fixAndPack res)
              , ("imports", "") ]
              -- empty for CAN
              -- "import STM32.Peripheral." <> tshow p <> ".RegTypes" ) ]
            writeHS ns t

            let newP = fix4PeripheralDefinition new
            -- peripheral definition
            (ns, t) <- procPeriphSpecificTemplate (tshow p) "STM32.Peripheral.X.Peripheral" Nothing
              [ ("type", tshow p)
              , ("bitDataRegs", fixAndPack $ ppBitDataRegs newP)
              , ("bitDataRegsMk", fixAndPack $ ppBitDataRegsMk newP)
              ]
            writeHS ns t

            -- reexports
            (ns, t) <- procPeriphTemplate (tshow p) "STM32.Peripheral.X" Nothing
              [ ("type", (tshow p)) ]
            writeHS ns t

          True -> do
            forM_ (filter (\(p', _, _) -> p' == p) representatives) $ \(_, ver, repre) -> do
              -- get representative for this peripheral version
              let x' = head . filter ((==periph2svdName p) . periphGroupName) $ devicePeripherals (get repre)

              -- register bitdata
              let new = procPeriph p (Just ver) x'
                  res = ppPeriphRegs new
              (ns, t) <- procPeriphTemplate (tshow p) "STM32.Peripheral.X.Regs" (Just $ tshow ver)
                [ ("regs", fixAndPack res)
                , ("imports", "import Ivory.BSP.STM32.Peripheral." <> tshow p <> versionedRegTypes p ver <> ".RegTypes" ) ]
              writeHS ns t

              let
                  common = [ "Regs" ]
                  commonForAllVersions = [
                      -- rename this to RegTypes
                      (UART, [ "RegTypes", "Pins" ])
                    ]
                  versionSpecific = [
                      (GPIO, [ "RegTypes", "TH" ])
                    ]

              -- additional files / regtypes
              -- take stuff from UART/file to UART1/file
              case (filter ((==p) . fst) commonForAllVersions) of
                [] -> return ()
                [(_, files)] -> forM_ files $ \file -> do
                  (ns, t) <- procPeriphSpecificTemplate (tshow p) ("STM32.Peripheral.X." <> file) Nothing []
                  writeHS ns t

              -- take stuff from UART1/file to UART1/file
              case (filter ((==p) . fst) versionSpecific) of
                [] -> return ()
                [(_, files)] -> forM_ files $ \file -> do
                  (ns, t) <- procPeriphSpecificTemplate (tshow p <> tshow ver) ("STM32.Peripheral.X." <> file) Nothing []
                  writeHS ns t

               -- peripheral definition
              (ns, t) <- procPeriphSpecificTemplate (tshow p <> tshow ver) "STM32.Peripheral.X.Peripheral" Nothing
                [ ("type", tshow p)
                , ("bitDataRegs", fixAndPack $ ppBitDataRegs new)
                , ("bitDataRegsMk", fixAndPack $ ppBitDataRegsMk new)
                , ("version", tshow ver)
                ]
              writeHS ns t
            -- /forM for periph representatives

              case p of
                GPIO -> do
                      -- reexports
                      (ns, t) <- procPeriphSpecificTemplate (tshow p) "STM32.Peripheral.X" Nothing
                        [ ("type", (tshow p)) ]
                      writeHS ns t

                UART -> do
                      -- reexports
                      (ns, t) <- procPeriphSpecificTemplate (tshow p) "STM32.Peripheral.X" Nothing
                        [ ("type", (tshow p)) ]
                      writeHS ns t

                _    -> do

                      -- reexports
                      (ns, t) <- procPeriphTemplate (tshow p) "STM32.Peripheral.X" Nothing
                        [ ("type", (tshow p) <> (tshow ver)) ]
                      writeHS ns t


  -- RCC reg types common for all devs
  (ns, t) <- procPeriphSpecificTemplate (tshow RCC) ("STM32.Peripheral.X.RegTypes") Nothing []
  writeHS ns t


versionedPeriphs = [GPIO, UART]
versioned x = x `elem` versionedPeriphs

versionedDrivers = [UART, SPI]
versionedDriver x = x `elem` versionedDrivers

versionedRegTypes GPIO ver = tshow ver
versionedRegTypes _ _ = mempty

representatives :: [(Periph, Int, String)]
representatives = [
  -- periph version representative
    (UART, 1, "F103")  -- no over8 feature (uart_cr1_over8)
  , (UART, 2, "F479")
  , (UART, 3, "F765")

  , (GPIO, 1, "F103")
  , (GPIO, 2, "F765")
  ]

canDualRegs :: [String]
canDualRegs = [
   "TDT[0-1]?R"
 , "TDL[0-1]?R"
 , "TDH[0-1]?R"
 , "TI[0-1]?R"
 , "RI[0-1]?R"
 , "RF[0-1]?R"
 , "RDT[0-1]?R"
 , "RDL[0-1]?R"
 , "RDH[0-1]?R"
 ]

adjustCANRegs x = x { periphRegisters = L.nubBy (\x y -> regName x == regName y) $ map merge $ periphRegisters x }
  where
    merge reg = foldl re reg canDualRegs
    re reg match | regName reg =~ match = reg { regName = replace "[0-1]?" "" match }
    re reg _ = reg

fix4PeripheralDefinition new = filterRegsByName (not . isDualReg) new
  where
    isDualReg x = or $ map (\match -> x =~ match) canDualRegs

-- bit arrays
-- FM1R FS1R FFA1R FA1R
canBitArrays = []

-- filters FiRx(32) FiRx(16)
-- F0R1 is Filter bank 0 register 1 up to F27Rx x in [1,2]
canFilters :: String
canFilters = "F[0-9][0-9]?R[1-2]"

-- UART
adjustUARTRegs x = adjustFields fix x
  where
    fix x | fieldName x == "DR"  = Just $ x { fieldBitWidth = 8 } -- we fix these to 8 bits, they are defined as 9 bit iirc but that fucks with our drivers
    fix x | fieldName x == "RDR" = Just $ x { fieldBitWidth = 8 }
    fix x | fieldName x == "TDR" = Just $ x { fieldBitWidth = 8 }
    fix x | fieldName x == "DIV_Mantissa" = Nothing  -- mantissa is dropped and div bellow covers both mantissa and fraction
    fix x | fieldName x == "DIV_Fraction" = Just $ x { fieldBitWidth = 16, fieldName = "div", fieldDescription = "divider" }
    fix x | fieldDescription x == "Word length" = Just $ x { fieldRegType = Just "UART_WordLen" }
    fix x | fieldDescription x == "STOP bits" = Just $ x { fieldRegType = Just "UART_StopBits" }
    fix x = Just x

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
    fix x | "GPIO" `L.isPrefixOf` x = ("GPIO"++) . drop 5 $ x
    fix x | "gpio" `L.isPrefixOf` x = ("gpio"++) . drop 5 $ x
    fix x | otherwise = x

adjustRCCRegs x = merges $ adjustFields fix x
  where
    fix x | "PPRE"    `L.isPrefixOf` (fieldName x)      = Just $ setFieldType "RCC_PPREx" x
    fix x | "HPRE"    `L.isPrefixOf` (fieldName x)      = Just $ setFieldType "RCC_HPRE" x
    fix x | fieldName x `matchesRe` "MCO[0-9]?PRE"      = Just $ setFieldType "RCC_MCOxPre" x
    fix x | fieldName x `matchesRe` "MCO[0-9]?"         = Just $ setFieldType "RCC_MCOx" x
    fix x | fieldName x == "SW" || fieldName x == "SWS" = Just $ setFieldType "RCC_SYSCLK" x
    fix x | otherwise                                   = Just x
    merges x = mergeFields "PLLP" [0..1] (setFieldType "RCC_PLLP")
             . mergeFields "PLLQ" [0..3] id
             . mergeFields "PLLN" [0..8] id
             . mergeFields "PLLM" [0..5] id
             $ x

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

filterByPeriph GPIO (Just 1) x = renameGPIO $ adjustGPIOF1Regs x
filterByPeriph GPIO (Just 2) x = renameGPIO $ adjustGPIORegs x
filterByPeriph CAN  _        x = dropID $ adjustCANRegs $ filterRegsByName (not . (=~ canFilters)) x
filterByPeriph UART _        x = dropID $ adjustUARTRegs x
filterByPeriph RCC  _        x = adjustRCCRegs x
filterByPeriph _    _        x = x

-- toplevel kindof
procPeriph p ver x = trace (show $ mapRegs (continuityCheck . regFields) lal) lal
  where
  lal = adjustRegs (\r -> r { regFields = procFields $ regFields r}) $ filterByPeriph p ver x

-- Special driver/peripheral regs versioning threatment
--
-- UART needs UART1 & UARTv2 for old/new periph regs, has common driver
-- USART -> UART
-- SPI has common regs but two drivers
-- CAN needs special threatment for filters, bitArrays and composed FIFOs

filterRegsByName f x = x { periphRegisters = filter (f . regName) $ periphRegisters x }

-- common STM32 support (src/Ivory/BSP/STM32 and src/Ivory/BSP/ARMv7M)
stm32toplevel = do
  m <- genMCUData "devices.data"
  -- MCU
  (ns, t) <- procHSTemplate "STM32.MCU" [ ("devices", T.pack m) ]
  writeHS ns t

  -- VectorTable
  (ns, t) <- procHSTemplate "STM32.VectorTable"
    [ ("imports", T.unlines $ familyImports "Interrupt")
    , ("byFamily", T.unlines $ map wwdgByFamily supFamilies) ]
  writeHS ns t

  -- copied verbatim
  let copies = [
          "Core"
        , "ClockConfig"
        , "ClockConfig.Init"
        , "Family"
        , "Interrupt"
        , "LinkerScript"
        ]

  mapM_ (\x -> copyHSTemplate $ T.concat ["STM32.", x]) copies

  cptree "../templates/ARMv7M/" "./src/Ivory/BSP/ARMv7M/"
  where
    wwdgByFamily fam = T.pack $ "byFamily " ++ (show fam) ++ " = attrs " ++ (show fam) ++ ".WWDG"

copyHSTemplate name = do
  (ns, t) <- procHSTemplate name []
  writeHS ns t

-- writeHS "STM32.Bla" "content"
writeHS namespace content = do
  here <- pwd
  cd "src"
  mktree tree
  cd tree
  TIO.writeFile fname content
  cd here
  where
    tree = fromString $ T.unpack $ T.replace "." "/" $ fst sp
    fname = fromString $ T.unpack $ T.concat [snd sp, ".hs"]
    sp = T.breakOnEnd "." namespace


normalizeISRNames xs = map (\x -> x { interruptName = norm (interruptName x) }) xs
  where norm name = replace "_IRQ" ""  -- for F0 we have WWDG_IRQ so normalize these
                  $ replace "lptim1_OR_it_eit_23" "LPTIM1_EXT1_23"
                    name


-- Right f103 <- parseSVD "data/STMicro/STM32F103xx.svd"

stm32families svds shortMCUs = forM supFamilies $ \f -> do
    putStrLn $ "Processing family " ++ (show f)
    let
      fns = "STM32" ++ (show f)
      svdsFam = svdsFamily f svds
      mcusFam = filter (L.isPrefixOf (show f)) shortMCUs
      isr = replaceOne "|" "="
          $ T.pack
          $ ppISRs
          $ normalizeISRNames
          $ isrsFamily f svds

    (ns, t) <- procFamilyTemplate f "STM32XX.Interrupt" [ ("isr", isr) ]
    writeHS ns t

    {-
     - memmap for family, not usable
    let mem = T.pack
          $ ppMemMap
          $ S.toList
          $ S.unions
          $ map (S.fromList . getDevMemMap) svdsFam

    (ns, t) <- procFamilyTemplate f "STM32XX.MemoryMap" [ ("mem", mem) ]
    writeHS ns t
    -}
