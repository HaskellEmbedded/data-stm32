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

-- w00t
-- [ ] merged regmaps for multiple periphs
-- [ ] generate register definition files
-- [ ] generate periph definition files
-- [ ] add drivers, case according to IP version
--
-- integrate
-- src/Data/Ivory/MCU.hs
-- src/Data/Ivory/MemMap.hs
--
-- src/Data/Ivory/Periph
--
-- [?] mcudiff
-- [ ] web


cdmk dir = do
  hasdir <- testdir dir
  when (not hasdir) (mktree dir)
  cd dir


supFamilies =
  [ F0
  , F1
  , F3
  , F4
  , F7 ]

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

fixRegName "GPIOB_OSPEEDR" = "OSPEEDR"
fixRegName x = x

fixRegs peri = peri { periphRegisters = mapRegs (\x -> x { regName = fixRegName $ regName x }) peri }

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

  cd dir
  (shortMCUs, families, mcuxmls) <- cmx

  --for STM32/XYPeriph/Regs
  --print $ mergedRegistersForPeriph "USART" svds

  cd dir
  stm32toplevel
  stm32periphs (snd . head . svdByMcu) -- (snd . head $ svdByMcu ("STM32F765" :: String))

  cd dir
  forM supFamilies $ \f -> do
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

    --print $ zip (map (length . svdByMcu) mcusFam) mcusFam
    --print $ zip (map svdByMcu mcusFam) mcusFam
    forM mcusFam $ \mcu -> do
      print $ (mcu, map fst  $ svdByMcu mcu)

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

  -- cabal file and support files
  cd dir
  mods <- prefixRest . L.sort . map (T.replace ".hs" "" . T.replace "/" "." . T.replace "./src/" "" . fpToText)
            <$> fold (find (suffix ".hs") "./src/") Fold.list
  t <- procTemplate "ivory-bsp-stm32.cabal" [("exposed", T.intercalate ",\n" mods)]
  TIO.writeFile "ivory-bsp-stm32.cabal" t
  cptree "../templates/support/" "./support/"
  where
    prefixRest (x:xs) = x:(map (\y -> T.concat [T.replicate (T.length "exposed-modules:       ") " ", y]) xs)

-- generate peripheral definitions (src/Ivory/BSP/STM32/Peripheral/
-- for all supPeriphs
stm32periphs get = do
  -- base non-versioned peripherals on this devices svd
  let nonVersionedBase = "STM32F765"
      nVDev = get nonVersionedBase

  forM_ supPeriphs $ \p -> do
    case filter ((==periph2svdName p) . periphGroupName) $ devicePeripherals nVDev of
      [] -> fail $ "No peripheral found with groupName " ++ show p ++ " for device " ++ (deviceName nVDev)
      --xs | length xs > 1 -> fail $ "Multiple peripheral found with groupName " ++ show p ++ " for device " ++ (deviceName devSvd)
      x:xs -> do

        case versioned p of
          False -> do
             -- register bitdata
            let new = fixRegs $ filterByPeriph p Nothing x
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
              let new = fixRegs $ filterByPeriph p (Just ver) x'
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
              -- TODO
              -- complete GPIO interface needs wrapping for two GPIO versions
              -- so scan for exports and wrap all functions

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

              -- reexports
              (ns, t) <- procPeriphTemplate (tshow p) "STM32.Peripheral.X" Nothing
                [ ("type", (tshow p) <> (tshow ver)) ]
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
  -- there is no uart_cr1_over8 :: Bit for F103??
    (UART, 1, "F479")
  , (UART, 2, "F765")
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
adjustUartRegs x = adjustFields fix x
  where
    fix x | fieldName x == "DR"  = Just $ x { fieldBitWidth = 8 }
    fix x | fieldName x == "RDR" = Just $ x { fieldBitWidth = 8 }
    fix x | fieldName x == "TDR" = Just $ x { fieldBitWidth = 8 }
    fix x | fieldName x == "DIV_Mantissa" = Nothing
    fix x | fieldName x == "DIV_Fraction" = Just $ x { fieldBitWidth = 16, fieldName = "div", fieldDescription = "divider" }
    fix x | fieldDescription x == "Word length" = Just $ x { fieldRegType = Just "UART_WordLen" }
    fix x | fieldDescription x == "STOP bits" = Just $ x { fieldRegType = Just "UART_StopBits" }
    fix x = Just x

adjustGPIORegs x = adjustFields fix x
  where
    fix x | "PUPDR"   `L.isPrefixOf` (fieldName x) = Just $ setRegType "GPIO_PUPD" x
    fix x | "AFR"     `L.isPrefixOf` (fieldName x) = Just $ setRegType "GPIO_AF" x
    fix x | "OSPEEDR" `L.isPrefixOf` (fieldName x) = Just $ setRegType "GPIO_Speed" x
    fix x | "OT"      `L.isPrefixOf` (fieldName x) = Just $ setRegType "GPIO_OutputType" x -- gpio_ot vs gpio_otype_x
    fix x | "MODE"    `L.isPrefixOf` (fieldName x) = Just $ setRegType "GPIO_Mode" x
    fix x = Just x

adjustGPIOF1Regs x = adjustFields fix x
  where
    fix x | "MODE"    `L.isPrefixOf` (fieldName x) = Just $ setRegType "GPIOF1_Mode" x
    fix x = Just x

setRegType rt x = x { fieldRegType = Just rt }

adjustFields fn x@Peripheral{..} = x { periphRegisters = map adj periphRegisters }
  where
    adj reg@Register{..} = reg { regFields = catMaybes $ map fn regFields }

filterByPeriph GPIO (Just 1) x = adjustGPIOF1Regs x
filterByPeriph GPIO (Just 2) x = adjustGPIORegs x
filterByPeriph CAN  _        x = adjustCANRegs $ filterRegsByName (not . (=~ canFilters)) x
filterByPeriph UART _        x = adjustUartRegs x
filterByPeriph _    _        x = x

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
--        , "ClockConfig.Init"
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
--
