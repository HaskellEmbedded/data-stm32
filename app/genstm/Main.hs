{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import Prelude hiding (log, FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import GHC.Generics hiding (L1)
import qualified Data.ByteString.Char8 as B

import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Map as M

import Text.Hastache
import Text.Regex.Posix
import Text.Pretty.Simple

import Data.Maybe
import Data.Ord (comparing)
import Data.Serialize (encode)

import Data.Ivory
import Data.Ivory.Pretty
import Data.Ivory.ISR
import Data.Ivory.MCU
import Data.Ivory.Periph

import Data.SVD hiding (svd, ppPeripheral)
import Data.CMX
import Data.STM32

import Debug.Trace

import Coerce
import Contexts
import Extract
import Template
import MakePeriph
import Types
import Utils

main = do
  seesData <- testdir "data"
  unless seesData $ error "Directory ./data not found, running from correct directory?"

  here <- pwd
  cd "data"
  dir <- pwd

  hasGarbage <- testdir "src"
  when hasGarbage $ do
    rmtree "src"
    rmtree "support"

  mktree "src"
  mktree "support"

  (tPath, cabalTemplate) <- runGen $ do
    dbStats
    stm32toplevel
    stm32periphs
    stm32modes
    stm32devs
    stm32families
    (,) <$> getTemplatesPath <*> getTemplate "ivory-bsp-stm32.cabal_template"

  -- cabal file and support files
  cd dir
  mods <- prefixRest
        . L.sort
        . map (T.replace ".hs" ""
              . T.replace "/" "."
              . T.replace "./src/" ""
              . fpToText)
      <$> fold (find (suffix ".hs") "./src/") Fold.list

  r <- hastacheStr hastacheConf cabalTemplate
         (listCtx [("exposed", T.intercalate ",\n" mods)])
  TLIO.writeFile "ivory-bsp-stm32.cabal" r

  cptree (fromText $ tPath <> "/support/") "./support/"

  cd here
  where
    prefixRest (x:xs) = x:(map (\y -> T.concat [T.replicate (T.length "exposed-modules:       ") " ", y]) xs)

stm32devs = do
  DB{..} <- ask
  devs <- filteredDevs
  forM_ devs $ \(name, mcu) -> do
    svd <- svdForMCU mcu

    log $ "Processing device " ++ (show (deviceName svd))
    -- memMap
    let ns = "STM32" <> (T.pack name) <> ".MemoryMap"
        ctx = listCtx [ ("dev", T.pack name)
                      , ("map" , T.pack $ ppMemMap $ getDevMemMap svd ) ]
    template ctx ns "STM32DEV/MemoryMap.hs"

    -- interrupts
    let isr = replaceOne "|" "="
            $ T.pack
            $ ppISRs
            $ normalizeISRNames
            $ renameDups
            $ isrs [svd]

    let ns = "STM32" <> (T.pack name) <> ".Interrupt"
        ctx = listCtx [ ("isr", isr) ]
    template ctx ns "STM32DEV/Interrupt.hs"

    let genPeriph p = case peripheralByGroupName svd p of
         Nothing -> fail $ "No " ++ (show p) ++ " found"
         Just svdPeriph -> genPeriphDev p svdPeriph name

    forM_ [ RCC, FLASH, PWR ] genPeriph

    log $ "Processing instances for " ++ name
    forM_ [ GPIO ] $ \periph -> do
      ctx <- makePeriphContext periph mcu
      let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow periph)]
      templateD ctx (target <> ".Ports") "STM32DEV/GPIO/Ports.hs"
      templateD ctx (target <> ".Pins")  "STM32DEV/GPIO/Pins.hs"
      templateD ctx target "STM32DEV/GPIO.hs"

    forM_ (supported L.\\ [GPIO]) $ \periph -> do
      when (hasPeriph mcu periph && hasDriver mcu periph) $ do
        ctx <- makePeriphContext periph mcu

        let
            pName = tshow periph
            target = T.concat ["STM32", (T.pack $ dev ctx), ".", pName]

        templateD ctx (T.concat ["STM32", (T.pack $ dev ctx), ".", pName])
                      (T.concat ["STM32DEV/", pName, ".hs"])

    let ns = "STM32" <> (T.pack name) <> ".Clock"
        clks = mcuClocks mcu
        ctx = ClocksCtx (map (\csrc -> ClockCtx (clockSourceName csrc) (show $ clockSourceHz csrc)) clks)
    templateD ctx ns "STM32DEV/Clock.hs"

    let ns = "STM32" <> (T.pack name) <> ".AF"
        ctx = listCtx [ ("module", ipShortName GPIO mcu) ]
    template ctx ns "STM32DEV/AF.hs"

    -- toplevel
    let ns = "STM32" <> (T.pack name)
        imports = map show $ filter (\periph -> hasPeriph mcu periph && hasDriver mcu periph) (supported)
        ctx = ImportsCtx name imports
    templateD ctx ns "STM32DEV.hs"

-- generate peripheral definitions (src/Ivory/BSP/STM32/Peripheral/
-- for all supportedPeriphs
stm32periphs = do
  DB{..} <- ask
  -- base non-versioned peripherals on this devices svd
  let nonVersionedBase = "F765"
      nVDev = get nonVersionedBase

  forM_ supported $ \p -> do
    forM_ (periphDrivers p) $ \di@DriverInfo{..} -> do
      let repre = case (diPeriph, diVersion) of
                 (GPIO, Just 2) -> head $ fx cmxs nonVersionedBase
                 _ -> someMCUWithDriver di $ cmxDevices cmxsWithSVD
      repreSVD <- get $ mcuRefName repre
      let
          svdPeriph = getPeriph (show p) repreSVD
          pName = usart $ tshow p
          nameVersion = pName <> (maybe mempty (("v"<>) . tshow) diVersion)
          usart "USART" = "UART"
          usart x = x

      -- Peripheral.RegTypes
      tPath <- getTemplatesPath
      regTypesImports <- case diRegTypes of
        NoRegTypes -> return []
        CommonRegTypes -> do
          -- this intentionally copies whole Peripheral.XYZ dir
          -- as it might contain other files common for this periph
          cptree (fromText $ tPath <> "/STM32/Peripheral/" <> pName)
                 (fromText $ "./src/Ivory/BSP/STM32/Peripheral/" <> pName)
          return $ [ T.concat ["import Ivory.BSP.STM32.Peripheral.", pName, ".RegTypes"]]
        VersionedRegTypes -> do
          mkdir $ fromText $ "./src/Ivory/BSP/STM32/Peripheral/" <> nameVersion
          cptree (fromText $ tPath <> "/STM32/Peripheral/" <> nameVersion)
                 (fromText $ "./src/Ivory/BSP/STM32/Peripheral/" <> nameVersion)
          return $ [ T.concat [ "import Ivory.BSP.STM32.Peripheral.", nameVersion, ".RegTypes"]]

      -- Peripheral.Regs
      new <- usartToUart <$> procPeriph p diVersion svdPeriph
      let
          res = ppPeriphRegs new
          ns = "STM32.Peripheral." <> nameVersion <> ".Regs"
          ctx = RegsCtx { imports = regTypesImports
                        , regs = T.pack res }

      templateD ctx ns "STM32/Peripheral/X/Regs.hs"

      -- Peripheral
      let ns = "STM32.Peripheral." <> nameVersion <> ".Peripheral"
          ctx = PeriphCtx {
                  typ = T.unpack $ pName
                  , bitDataRegs = ppBitDataRegs new
                  , bitDataRegsMk = ppBitDataRegsMk new
                  , pversion = maybe "" (('v':) . show) diVersion
                  }
      templateD ctx ns ("STM32/Peripheral/" <> nameVersion <> "/Peripheral.hs")

      -- Toplevel peripheral
      let ns = "STM32.Peripheral." <> pName
          vers = catMaybes $ driverVersions p
          versData = map (\(v, fst) -> VersionCtx
                          { version = show v
                          , prefix = if fst then " " else "|" })
                          $ zip vers (True: [False, False ..])

          ctx = VersionsCtx $ versData
      templateD ctx ns $ "STM32/Peripheral/" <> pName <> ".hs"

      -- Driver
      let
          ns = "STM32.Driver." <> nameVersion

      unless (diDriver == NoDriver) $ do
        template' ns $ "STM32/Driver/" <> nameVersion <> ".hs"

        tPath <- getTemplatesPath
        driverDir <- testdir $ fromText $ tPath <> "/STM32/Driver/" <> pName
        when driverDir $ do
          cptree (fromText $ tPath <> "/STM32/Driver/" <> pName)
                 (fromText $ "./src/Ivory/BSP/STM32/Driver/" <> pName)

        -- Toplevel driver
        let ns = "STM32.Driver." <> pName
            vers = catMaybes $ driverVersions p
            versData = map (\(v, fst) -> VersionCtx
                            { version = show v
                            , prefix = if fst then " " else "|" })
                            $ zip vers (True: [False, False ..])

            ctx = VersionsCtx $ versData
        templateD ctx ns $ "STM32/Driver/" <> pName <> ".hs"

  -- RCC reg types common for all devs
  template'
    "STM32.Peripheral.RCC.RegTypes"
    "STM32/Peripheral/RCC/RegTypes.hs"

stm32modes = do
  DB{..} <- ask

  forM_ (M.toList afs) $ \(name, xs) -> do
    -- alternate functions
    let ns = "STM32.AF." <> (T.pack name)
        ctx = listCtx [ ("afs", show xs) ]
    template ctx ns "STM32/Modes/AF.hs"

strip = M.map go
  where go x = x { mcuIps = S.empty, mcuPins = S.empty }

-- common STM32 support (src/Ivory/BSP/STM32 and src/Ivory/BSP/ARMv7M)
stm32toplevel :: MonadGen ()
stm32toplevel = do
  log "Toplevel"
  db <- ask

  let slim = strip $ nameMapped db
  liftIO $ B.writeFile "devices.data" (encode slim)

  -- MCU
  template' "STM32.MCU" "STM32/MCU.hs"

  -- VectorTable
  devs <- filteredShortNames
  let devctx = ShortDevicesCtx devs
  templateD devctx "STM32.VectorTable" "STM32/VectorTable.hs"

  -- ClockInit
  let fctx = FamiliesCtx $ map show supportedFamilies
  templateD fctx "STM32.ClockInit" "STM32/ClockInit.hs"

  -- copied verbatim
  let copies = [
          "ClockConfig"
        , "Interrupt"
        , "LinkerScript"
        , "AF"
        ]

  mapM_ (\x -> template' ("STM32." <> x) $ T.concat ["STM32/", x, ".hs"]) copies

  liftIO $ cptree "../templates/ARMv7M/" "./src/Ivory/BSP/ARMv7M/"

stm32families :: MonadGen ()
stm32families = do
  log "Families"
  forM_ supportedFamilies $ \f -> do
    svdsFam <- svdsFamily f
    svdSet <- S.fromList <$> svdsFamily f

    log $ "Processing family " ++ (show f)
    log $ "Distinct SVDs for this family "
      ++ (show . S.size $ svdSet)
      ++ " "
      ++ (show . S.map deviceName $ svdSet)

    let isr = replaceOne "|" "="
            $ T.pack
            $ ppISRs
            $ normalizeISRNames
            $ renameDups
            $ isrs
            $ svdsFam

    template (listCtx [ ("isr", isr) ])
      ("STM32" <> tshow f <> ".Interrupt")
       "STM32FAM/Interrupt.hs"

    template (listCtx [ ("fam", tshow f) ])
      ("STM32" <> tshow f <> ".ClockInit")
      ("STM32FAM/" <> tshow f <> "/ClockInit.hs")

    dev <- last <$> svdsFamily f
    log $ "Device used as family representative (RCC, ClockInit) " ++ deviceName dev

    let genPeriph p = case peripheralByGroupName dev p of
         Nothing -> fail $ "No " ++ (show p) ++ " found"
         Just svdPeriph -> genPeriphFamily p svdPeriph f

    forM_ [ RCC, FLASH, PWR ] genPeriph

    let ns = "STM32" <> tshow f <> ".MemoryMap"
        ctx = listCtx [ ("dev", tshow f)
              , ("map" , T.pack $ ppMemMap $ getDevMemMap dev ) ]
    template ctx ns "STM32DEV/MemoryMap.hs"

-- templating of non-versioned peripherals like RCC, PWR, FLASH
genPeriphDev :: Periph -> Peripheral -> String -> MonadGen ()
genPeriphDev periph svdPeripheral dev =
  genPeriphCommon periph svdPeripheral
    id (T.pack dev)

genPeriphFamily :: Periph -> Peripheral -> Family -> MonadGen ()
genPeriphFamily periph svdPeripheral family =
  genPeriphCommon periph svdPeripheral
    (adjustPeriphFamily family) (tshow family)

genPeriphCommon :: Periph
                -> Peripheral
                -> (Peripheral -> Peripheral)
                -> Text
                -> MonadGen ()
genPeriphCommon periph svdPeripheral adjust devName = do
  new <- fmap adjust $ procPeriph periph Nothing svdPeripheral
  checkPeriphRegsContinuity svdPeripheral new
  let pName = tshow periph
      ns = "STM32" <> devName <> "." <> pName
      ctx = listCtx [
              ("dev",  devName)
            , ("regs", T.concat [ T.pack $ ppPeriphRegsWithDefs new])
            ]
  template ctx ns ("STM32DEV/" <> pName <> ".hs")

tst = do
  runGen $ do
    db <- ask
    let
      f103 = head $ fx (cmxs db) "F103"
      f427 = head $ fx (cmxs db) "F427"

    f103svd <- svdForMCU f103
    f427svd <- svdForMCU f427
    return (f103svd, f427svd)
