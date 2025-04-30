{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import Prelude hiding (log, FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import Data.Char (toUpper)
import qualified Data.List.NonEmpty
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import GHC.Generics hiding (L1)
import qualified Data.ByteString.Char8 as B

import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Map as M

import Text.Mustache
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
import MakeDMAUART
import Types
import Utils

main :: IO ()
main = do
  seesData <- testdir "data"
  unless seesData $ error "Directory ./data not found, running from correct directory?"

  here <- pwd
  cd "data"
  dataDir <- pwd

  hasGarbage <- testdir "src"
  when hasGarbage $ do
    rmtree "src"
    rmtree "support"

  mktree "src"
  mktree "support"

  -- generated stuff
  runGen $ do
    dbStats
    stm32toplevel
    stm32periphs
    stm32modes
    stm32devs
    stm32families
    readme

    -- cabal file
    liftIO $ cd dataDir
    mods <- liftIO
        $ prefixRest
        . L.sort
        . map (T.replace ".hs" ""
              . T.replace "/" "."
              . T.replace "./src/" ""
              . fpToText)
      <$> fold (find (suffix ".hs") "./src/") Fold.list

    r <- templateRaw
           (listCtx [("exposed", T.intercalate ",\n" mods)])
           "ivory-bsp-stm32.cabal_template"
    liftIO $ TIO.writeFile "ivory-bsp-stm32.cabal" r

    tPath <- getTemplatesPath
    liftIO $ cptree (T.unpack $ tPath <> "/support") "support"

  cd here
  where
    prefixRest (x:xs) = x:(map (\y -> T.concat [T.replicate (T.length "exposed-modules:       ") " ", y]) xs)

stm32devs :: MonadGen ()
stm32devs = do
  DB{..} <- ask
  devs <- filteredDevs
  forM_ devs $ \(name, mcu) -> do
    svd <- svdForMCU mcu

    log $ "Processing device " ++ (show (deviceName svd))
    -- memMap
    let ns = "STM32" <> (T.pack name) <> ".MemoryMap"
        ctx = listCtx [ ("dev", T.pack name)
                      , ("map" , T.pack $ displayMemMap $ addSYSCFGAlias $ getDevMemMap svd ) ]
    template ctx ns "STM32DEV/MemoryMap.hs"

    -- interrupts
    let isr = replaceOne "|" "="
            $ T.pack
            $ displayISRs
            $ normalizeISRNames
            $ renameDups
            $ isrs [svd]

    let ns = "STM32" <> (T.pack name) <> ".Interrupt"
        ctx = listCtx [ ("isr", isr) ]
    template ctx ns "STM32DEV/Interrupt.hs"

    let notFound p = log $ "- " ++ (show p) ++ " not found"
        genPeriph p = case peripheralByName svd p of
         Nothing -> notFound p
         Just svdPeriph -> genPeriphDev p svdPeriph name

    forM_ [ RCC, FLASH, PWR ] genPeriph

    log $ "Processing instances for " ++ name
    forM_ [ GPIO ] $ \periph -> do
      mCtx <- makePeriphContext periph mcu
      case mCtx of
        Nothing -> error "Empty periph context"
        Just ctx -> do
          let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow periph)]
          template ctx (target <> ".Ports") "STM32DEV/GPIO/Ports.hs"
          template ctx (target <> ".Pins")  "STM32DEV/GPIO/Pins.hs"
          template ctx target "STM32DEV/GPIO.hs"

    -- for F1 we have AFIO
    -- for G0 we have EXTICR regs in EXTI itself
    unless (mcuFamily mcu == F1 || mcuFamily mcu == G0) $ do
      forM_ [ EXTI ] $ \periph -> do
        ctx <- maybe (error "Empty periph context") id <$> makePeriphContext periph mcu
        let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow periph)]
        template ctx target "STM32DEV/EXTI.hs"

    when (mcuFamily mcu == G0) $ do
      forM_ [ EXTI ] $ \periph -> do
        ctx <- maybe (error "Empty periph context") id <$> makePeriphContext periph mcu
        let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow periph)]
        template ctx target "STM32DEV/EXTIG0.hs"

    when (mcuFamily mcu == F1) $ do
      forM_ [ EXTI ] $ \periph -> do
        ctx <- maybe (error "Empty periph context") id <$> makePeriphContext periph mcu
        let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow periph)]
        template ctx target "STM32DEV/EXTIF1.hs"

    -- special treatment due to G0s having SYSCFG_ITLINE in SYSCFG group
    -- and F303 having SYSCFG_COMP_OPAMP (as groupName /o\)
    case filter (\x -> (
                           (periphName x /= "SYSCFG_ITLINE"
                            && ((==show SYSCFG) . map toUpper . periphGroupName $ x))
                        || (periphGroupName x == "SYSCFG_COMP_OPAMP"))
                ) $ devicePeripherals svd of
      []  -> notFound SYSCFG
      [svdPeriph] -> do
          let di = DriverInfo SYSCFG Nothing (ByDev "") [] NoRegTypes NoDriver
          genPeriphTree (T.concat ["STM32", (T.pack name)]) SYSCFG di svdPeriph

          ctx <- maybe (error "Empty periph context") id <$> makePeriphContext SYSCFG mcu
          let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow SYSCFG)]
          template ctx target "STM32DEV/SYSCFG.hs"
      _   -> error "Multiple SYSCFGs"

    -- F1 AFIO
    when (mcuFamily mcu == F1) $ do
      let p = AFIO
      case peripheralByName svd p of
        Nothing -> notFound p
        Just svdPeriph -> do
          let di = DriverInfo AFIO Nothing (ByDev "F103") [] NoRegTypes NoDriver
          genPeriphTree (T.concat ["STM32", (T.pack name)]) AFIO di svdPeriph

          ctx <- maybe (error "Empty periph context") id <$> makePeriphContext AFIO mcu
          let target = T.concat ["STM32", (T.pack $ dev ctx), ".", (tshow AFIO)]
          template ctx target "STM32DEV/AFIO.hs"

    forM_ (supported L.\\ [GPIO, EXTI]) $ \periph -> do
      unless (hasPeriph mcu periph) $ log $ "- " ++ show periph ++ " hasPeriph = False"
      unless (hasDriver mcu periph) $ log $ "- " ++ show periph ++ " hasDriver = False"
      when (hasPeriph mcu periph && hasDriver mcu periph) $ do
        mCtx <- makePeriphContext periph mcu

        case mCtx of
          Nothing -> log "Empty periph context, skipping"
          Just ctx -> do
            let
              pName = tshow periph
              target = T.concat ["STM32", (T.pack $ dev ctx), ".", pName]

            template ctx
              (T.concat ["STM32", (T.pack $ dev ctx), ".", pName])
              (T.concat ["STM32DEV/", pName, ".hs"])

    -- ATIM/GTIM
    hasTims <- forM [ ATIM, GTIM ] $ \tim -> do
      timInst <- timerInstancesData tim mcu
      unless (null timInst) $ do
        template
          (TimersCtx
            { timDev = name
            , timInstances = timInst
            , timInstances32Bit = filter (\TimerInstanceCtx{..} -> timIndex `elem` [2, 5]) timInst
            }
          )
          (T.concat ["STM32", (T.pack name), ".", tshow tim])
          (T.concat ["STM32DEV/", tshow tim, ".hs"])
      pure $ not $ null timInst

    -- DMA U(S)ARTs for F4/F7s
    hasDmaUarts <- forM [ UART, USART ] $ \up -> do
        case mcuFamily mcu of
          fam | fam `elem` [F4, F7] -> do
            let
              pName = tshow up

            uartsCtx <- makePeriphContext up mcu
            case uartsCtx of
              Nothing -> pure False
              Just ctx -> do
                template
                  (fromInstancesCtx pName ctx)
                  (T.concat ["STM32", (T.pack $ dev ctx), ".", pName, ".DMA"])
                  (T.concat ["STM32DEV/UART/DMA.hs"])
                pure True

          otherwise -> pure False

    -- Ethernet for F4/F7s
    hasEthernet <-
      if
        ( "ETH" `S.member` S.map ipInstanceName (mcuIps mcu)
        && mcuFamily mcu `elem` [F4, F7]
        )
      then do
        template
          (listCtx
            -- eew
            $ pure ("dev", T.pack $ L.take 4 $ L.drop 5 $ mcuRefName mcu)
          )
          (T.concat ["STM32", (T.pack name), ".ETH"])
          (T.concat ["STM32DEV/ETH.hs"])

        pure True
      else
        pure False

    let ns = "STM32" <> (T.pack name) <> ".Clock"
        clks = mcuClocks mcu
        ctx = ClocksCtx (map (\csrc -> ClockCtx (clockSourceName csrc) (show $ clockSourceHz csrc)) clks)
    template ctx ns "STM32DEV/Clock.hs"

    let ns = "STM32" <> (T.pack name) <> ".AF"
        ctx = listCtx [ ("module", T.pack $ ipShortName GPIO mcu) ]
    template ctx ns "STM32DEV/AF.hs"

    -- toplevel
    let ns = "STM32" <> (T.pack name)
        sup = filter
          (\periph -> hasPeriph mcu periph
                   && hasDriver mcu periph)
          supported

    sup' <-
      filterM
        (\periph -> isJust <$> makePeriphContext periph mcu)
        sup
    let
      imports =
        map show sup'
        ++ case hasEthernet of
            True -> pure "ETH"
            False -> mempty
        ++ case hasDmaUarts of
            [True, True] -> [ "UART.DMA", "USART.DMA" ]
            [False, True] -> pure "USART.DMA"
            [True, False] -> pure "UART.DMA"
            _ -> mempty
        ++ case hasTims of
            [True, True] -> [ "ATIM", "GTIM" ]
            [False, True] -> pure "GTIM"
            [True, False] -> pure "ATIM"
            _ -> mempty
      ctx = ImportsCtx name imports
    template ctx ns "STM32DEV.hs"

-- generate peripheral definitions (src/Ivory/BSP/STM32/Peripheral/
-- for all supportedPeriphs
stm32periphs :: MonadGen ()
stm32periphs = do
  DB{..} <- ask
  -- base non-versioned peripherals on this devices svd
  let nonVersionedBase = "F765"

  forM_ supported $ \p -> do
    forM_ (filter ((/=LPUART) . diPeriph) $ periphDrivers p) $ \di@DriverInfo{..} -> do
      let repre = case (diPeriph, diVersion) of
                 (GPIO, Just 2) -> head $ fx cmxs nonVersionedBase
                 _ -> someMCUWithDriver di $ cmxDevices cmxsWithSVD

      repreSVD <- get $ mcuRefName repre
      log $ "Representative for "
            ++ show p
            ++ (maybe "" show diVersion)
            ++ " chosen "
            ++ mcuRefName repre
            ++ " IP "
            ++ getIPVersion p repre

      let
          svdPeriph = getPeriphByGroup (show p) repreSVD
          pName = usart $ tshow p
          nameVersion = case diDriver of
            CommonDriver -> pName
            _            -> pName <> (maybe mempty (("v"<>) . tshow) diVersion)
          usart "USART" = "UART"
          usart x = x

      genPeriphTree "STM32.Peripheral" p di svdPeriph

      -- Driver
      let
          ns = "STM32.Driver." <> nameVersion

      unless (diDriver == NoDriver) $ do
        template' ns $ "STM32/Driver/" <> nameVersion <> ".hs"

        tPath <- getTemplatesPath
        driverDir <- testdir $ T.unpack $ tPath <> "/STM32/Driver/" <> pName
        when driverDir $ do
          cptree (T.unpack $ tPath <> "/STM32/Driver/" <> pName)
                 (T.unpack $ "./src/Ivory/BSP/STM32/Driver/" <> pName)

        -- Toplevel driver
        let ns = "STM32.Driver." <> pName
            vers = catMaybes $ driverVersions p
            versData = map (\(v, fst) -> VersionCtx
                            { version = show v
                            , prefix = if fst then " " else "|" })
                            $ zip vers (True: [False, False ..])

            ctx = VersionsCtx $ versData
        template ctx ns $ "STM32/Driver/" <> pName <> ".hs"

  -- RCC reg types common for all devs
  template'
    "STM32.Peripheral.RCC.RegTypes"
    "STM32/Peripheral/RCC/RegTypes.hs"

  -- Timers (ATIM, GTIM) common for all devs (hopefully)
  -- no templating here
  tPath <- getTemplatesPath
  forM_ ["ATIM", "GTIM"] $ \tim -> do
    cptree
      (T.unpack $ tPath <> "/STM32/Peripheral/" <> tim)
      (T.unpack $ "./src/Ivory/BSP/STM32/Peripheral/" <> tim)
    template'
      ("STM32.Peripheral." <> tim)
      ("STM32/Peripheral/" <> tim <> ".hs")

  -- F4/F7 Ethernet
  nVDev <- get nonVersionedBase
  genEthernetPeriphTree nVDev
  template'
    ("STM32.Driver.ETH")
    ("STM32/Driver/ETH.hs")
  cptree
    (T.unpack $ tPath <> "/STM32/Driver/ETH")
    (T.unpack $ "./src/Ivory/BSP/STM32/Driver/ETH")

stm32modes :: MonadGen ()
stm32modes = do
  DB{..} <- ask

  forM_ (M.toList afs) $ \(name, xs) -> do
    -- alternate functions
    let ns = "STM32.AF." <> (T.pack name)
        ctx = listCtx [ ("afs", tshow xs) ]
    template ctx ns "STM32/Modes/AF.hs"

-- generate stripped down version of MCU Map
-- to be bundled with generated library
strip :: M.Map k MCU -> M.Map k MCU
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
  template devctx "STM32.VectorTable" "STM32/VectorTable.hs"

  -- ClockInit
  let fctx = FamiliesCtx $ map show supportedFamilies
  template fctx "STM32.ClockInit" "STM32/ClockInit.hs"

  -- copied verbatim
  let copies = [
          "ClockConfig"
        , "Interrupt"
        , "LinkerScript"
        , "AF"
        ]

  mapM_ (\x -> template' ("STM32." <> x) $ T.concat ["STM32/", x, ".hs"]) copies

  liftIO $ cptree "../templates/ARMv7M" "./src/Ivory/BSP/ARMv7M"

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
            $ displayISRs
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
              , ("map" , T.pack $ displayMemMapCompact $ addSYSCFGAlias $ getDevMemMap dev ) ]
    template ctx ns "STM32DEV/MemoryMap.hs"

-- iff there's no syscfg memory address we
-- try to create an alias from composed peripheral address e.g. syscfg_comp
addSYSCFGAlias x | "SYSCFG" `elem` (map snd x) = x
addSYSCFGAlias x | otherwise = case filter (\(adr, name) -> "SYSCFG" `L.isPrefixOf` name) x of
  [(adr, _)] -> (adr, "SYSCFG"):x
  _ -> x

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

genPeriphTree target p di svdPeriph = do
  let
      pName = usart $ tshow p
      nameVersion = case diDriver di of
        CommonDriver -> pName
        _            -> pName <> (maybe mempty (("v"<>) . tshow) (diVersion di))
      usart "USART" = "UART"
      usart x = x

  -- Peripheral.RegTypes
  tPath <- getTemplatesPath
  -- XXX: fix paths for `target` here as well
  regTypesImports <- case diRegTypes di of
    NoRegTypes -> return []
    CommonRegTypes -> do
      -- this intentionally copies whole Peripheral.XYZ dir
      -- as it might contain other files common for this periph
      cptree (T.unpack $ tPath <> "/STM32/Peripheral/" <> pName)
             (T.unpack $ "./src/Ivory/BSP/STM32/Peripheral/" <> pName)
      return $ [ T.concat ["import Ivory.BSP.STM32.Peripheral.", pName, ".RegTypes"]]
    VersionedRegTypes -> do
      mktree $ T.unpack $ "./src/Ivory/BSP/STM32/Peripheral/" <> nameVersion
      cptree (T.unpack $ tPath <> "/STM32/Peripheral/" <> nameVersion)
             (T.unpack $ "./src/Ivory/BSP/STM32/Peripheral/" <> nameVersion)
      return $ [ T.concat [ "import Ivory.BSP.STM32.Peripheral.", nameVersion, ".RegTypes"]]

  -- Peripheral.Regs
  new <-
        usartToUart
    <$> procPeriph p (diVersion di) svdPeriph
  let
      res = ppPeriphRegs new
      ns  = target <> "." <> nameVersion <> ".Regs"
      ctx = RegsCtx { imports = regTypesImports
                    , regs = T.pack res }

  unless (p == DMA) $ -- skipping regs templating should be part of DriverInfo
    template ctx ns "STM32/Peripheral/X/Regs.hs"

  -- Peripheral
  let ns = target <> "." <> nameVersion <> ".Peripheral"
      ctx = PeriphCtx
              { typ = T.unpack $ pName
              , bitDataRegs = ppBitDataRegs new
              , bitDataRegsMk = ppBitDataRegsMk new
              , pversion = maybe "" (('v':) . show) (diVersion di)
              }
  template ctx ns ("STM32/Peripheral/" <> nameVersion <> "/Peripheral.hs")

  -- Toplevel peripheral
  let ns = target <> "." <> pName
      vers = catMaybes $ driverVersions p
      versData = map (\(v, fst) -> VersionCtx
                      { version = show v
                      , prefix = if fst then " " else "|" })
                      $ zip vers (True: [False, False ..])

      ctx = VersionsCtx $ versData
  template ctx ns $ "STM32/Peripheral/" <> pName <> ".hs"

genEthernetPeriphTree repreSVD = do
  -- toplevel ETH
  tPath <- getTemplatesPath

  cptree
    (T.unpack $ tPath <> "/STM32/Peripheral/ETH")
    (T.unpack $ "./src/Ivory/BSP/STM32/Peripheral/ETH")

  template'
    "STM32.Peripheral.ETH"
    "STM32/Peripheral/ETH.hs"

  forM_
    [ ETHERNET_DMA
    , ETHERNET_MAC
    , ETHERNET_MMC
    , ETHERNET_PTP
    ] $ \p ->
    do
      case peripheralByName repreSVD p of
        Nothing -> log $ "- " ++ (show p) ++ " not found"
        Just svdPeriph -> do
          genEthPeriph p svdPeriph

genEthPeriph p svdPeriph = do
  let
      pName = eth $ tshow p
      target = "STM32.Peripheral.ETH"
      eth "ETHERNET_MAC" = "MAC"
      eth "ETHERNET_MMC" = "MMC"
      eth "ETHERNET_PTP" = "PTP"
      eth "ETHERNET_DMA" = "DMA"
      eth x = x

  -- Peripheral.RegTypes
  tPath <- getTemplatesPath
  cptree (T.unpack $ tPath <> "/STM32/Peripheral/ETH/" <> pName)
         (T.unpack $ "./src/Ivory/BSP/STM32/Peripheral/ETH/" <> pName)

  -- Peripheral.Regs
  new <-
        shortEth
    <$> procPeriph p Nothing svdPeriph
  let
    res = ppPeriphRegs new
    ns  = target <> "." <> pName <> ".Regs"
    ctx =
      RegsCtx
        { imports =
            pure
            $ T.concat
                [ "import Ivory.BSP.STM32.Peripheral.ETH."
                , pName
                , ".RegTypes"
                ]
        , regs = T.pack res
        }

  template
    ctx
    ns
    "STM32/Peripheral/X/Regs.hs"

  -- Peripheral
  let
    ns = target <> "." <> pName <> ".Peripheral"
    ctx =
      PeriphCtx
        { typ = T.unpack $ pName
        , bitDataRegs = ppBitDataRegs new
        , bitDataRegsMk = ppBitDataRegsMk new
        , pversion = ""
        }

  template
    ctx
    ns
    ("STM32/Peripheral/ETH/" <> pName <> "/Peripheral.hs")

readme = do
  DB{..} <- ask
  devs <- filteredDevs
  let header = "Device|" ++ (L.intercalate "|" $ map show $ L.sort supported)
      sep = "--|" ++ (L.intercalate "|" $ map (pure "--") $ L.sort supported)
      row dev sup = L.intercalate "|" (dev:sup)
      isSup mcu periph = hasPeriph mcu periph && hasDriver mcu periph
      supStr True  = "✓"
      supStr False = " "
      devSup = map (\(name, mcu) -> row name (map (supStr . isSup mcu) (L.sort supported))) devs

      all = unlines (header:sep:devSup)

  r <- templateRaw (listCtx [("matrix", T.pack all)]) "README.md"
  liftIO $ TIO.writeFile "README.md" r

tst = do
  runGen $ do
    db <- ask
    let
      f103 = head $ fx (cmxs db) "F103"
      f427 = head $ fx (cmxs db) "F427"

    f103svd <- svdForMCU f103
    f427svd <- svdForMCU f427
    return (f103svd, f427svd)
