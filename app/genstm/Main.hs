{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Turtle
import Prelude hiding (FilePath)
import Control.Monad
import qualified Control.Foldl as Fold
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics hiding (L1)
import Data.Serialize
import qualified Data.ByteString.Char8 as B

import qualified Data.List as L
import qualified Data.Map as M

import Text.Regex.Posix
import Text.Pretty.Simple

import Data.Char (toUpper)
import Data.Maybe
import Data.Ord (comparing)

import Data.Ivory
import Data.Ivory.Pretty
import Data.Ivory.ISR
import Data.Ivory.MCU
import Data.Ivory.MemMap
import Data.Ivory.Periph

import Data.SVD hiding (svd, ppPeripheral)
import Data.CMX
import Data.STM32.Types

import Debug.Trace

import Extract
import Utils


import Text.Karver
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V

lit = Literal
litList = List . V.fromList . map lit
litObj x = List . V.fromList $ [ elemToObj e isFirst | (e, isFirst) <- zip x [False, True ..] ]
  where
    elemToObj e isFirst = Object (H.fromList [ ("prefix", if isFirst then "|" else " "), ("version", e) ])

cdmk dir = do
  hasdir <- testdir dir
  when (not hasdir) (mktree dir)
  cd dir

periph2svdName :: Periph -> String
periph2svdName UART = "USART"
periph2svdName x = show x

-- crap
fixAndPack = T.replace "usart" "uart"
 . T.replace "USART" "UART"
 . T.pack


getTemplatesPath = do
  mPath <- need "TEMPLATES_PATH"
  case mPath of
    Nothing -> die "need TEMPLATES_PATH env var"
    Just p -> return p

getTemplate x = do
  tPath <- getTemplatesPath
  TIO.putStrLn $ "Loading template " <> x <> " from " <> tPath
  TIO.readFile $ fromString $ T.unpack $ tPath <> "/" <> x

loadDatabases = do
  mPath <- need "DB_PATH"
  dbPath <- case mPath of
    Nothing -> die "need DB_PATH env var"
    Just p -> return $ fromText p
  svds <- extractSVDCached dbPath
  cmxs <- extractCMXCached dbPath
  let
    supp = filterSupported cmxs
    cmxsWithSVD = filterHavingSVD (supp, svds)
    noSVD = (cmxDevices supp L.\\ cmxDevices cmxsWithSVD)
  return DB{..}

dbStats DB{..} = do
  let hasSVD = length $ catMaybes $ map (getSVDMaybe svds . mcuRefName) (cmxDevices cmxs)
      tot = length $ cmxDevices cmxs
      fams = length $ cmxFamilies cmxs
      supFams = length $ cmxFamilies $ filterSupported cmxs
  putStrLn $ unlines [
      "Families " ++ (show $ fams)
    , "Supported families " ++ (show $ supFams) ++ " " ++ (show supportedFamilies)
    , "Un-supported families " ++ (show $ fams - supFams) ++ " " ++ (show (cmxFamilies cmxs L.\\ supportedFamilies))
    , "CMX devices " ++ (show $ tot)
    , "SVD files " ++ (show $ length $ svds)
    , "CMX devices with SVD files " ++ (show $ hasSVD)
    , "CMX devices missing SVD files " ++ (show $ tot - hasSVD)
    , "SVD file names " ++ (show $ map fst svds)
    , "Supported but missing SVD files "
    ] ++ unlines (map mcuRefName noSVD)

filterHavingSVD (cmxs, svds) = M.map (filter (isJust . getSVDMaybe svds . mcuRefName)) cmxs

-- svd names look like STM32F41x, we replace x with [0-9A] so we can regex match on them
-- prefer longer pattern
-- (A in patter match due to L4A6 which hopefully matches L4x6.svd)
svdByMcu svds m = reverse
           $ L.sortBy (comparing fst)
           $ filter (\(name, dev) ->
                        (m =~ (replace "x" "[0-9A]" $ take 4 name)) :: Bool) svds
getSVDMaybe svds m = case svdByMcu svds m of
           [] -> Nothing
           x:xs -> Just $ snd x


getSVD :: [(String, Device)] -> String -> Device
getSVD svds = fromJust . getSVDMaybe svds

data DB = DB {
    cmxs :: M.Map Family [MCU]
  , cmxsWithSVD :: M.Map Family [MCU]
  , noSVD :: [MCU]
  , svds :: [(String, Device)]
  } deriving (Show)

main = do
  cd "data"
  dir <- pwd

  hasGarbage <- testdir "src"
  when hasGarbage $ do
    rmtree "src"
    rmtree "support"

  mktree "src"
  mktree "support"

  db <- loadDatabases
  dbStats db

  --for STM32XY/Periph/Regs
  --print $ mergedRegistersForPeriph "USART" svds

  cd dir
  stm32toplevel db
  stm32periphs db
  stm32devs db
  stm32families db

  -- cabal file and support files
  cd dir
  mods <- prefixRest
        . L.sort
        . map (T.replace ".hs" ""
              . T.replace "/" "."
              . T.replace "./src/" ""
              . fpToText)
      <$> fold (find (suffix ".hs") "./src/") Fold.list

  t <- getTemplate "ivory-bsp-stm32.cabal_template"
  TIO.writeFile "ivory-bsp-stm32.cabal" $
    renderTemplate (H.fromList [("exposed", lit $ T.intercalate ",\n" mods)]) t

  tPath <- getTemplatesPath
  cptree (fromText $ tPath <> "/support/") "./support/"
  where
    prefixRest (x:xs) = x:(map (\y -> T.concat [T.replicate (T.length "exposed-modules:       ") " ", y]) xs)

thesePlease :: [String]
thesePlease =
  [ "F103"
  , "F427"
  , "L432" ]

stm32devs DB{..} = do
  let
    get = getSVD svds
  forM (map (\x -> (x, get $ "STM32" ++ x)) thesePlease) $ \(namePart, dev) -> do
    putStrLn $ "Processing device " ++ (show (deviceName dev))
    -- memMap
    let ns = "STM32" <> (T.pack namePart) <> ".MemoryMap"
        ctx = [ ("dev", lit $ T.pack namePart )
              , ("map" , lit $ T.pack $ ppMemMap $ getDevMemMap dev ) ]
    template ctx ns "STM32DEV/MemoryMap.hs"

    let genPeriph p = case filter ((==periph2svdName p) . map toUpper . periphGroupName) $ devicePeripherals dev of
         [] -> fail $ "No " ++ (show p) ++ " found"
         [x] -> do
                let new = procPeriph p Nothing x
                let pName = T.pack . show $ p
                    ns = "STM32" <> (T.pack namePart) <> "." <> pName
                    ctx = [ ("dev", lit $ T.pack namePart )
                          , ("regs", lit $ T.concat [ T.pack $ ppPeriphRegsWithDefs new]) ]
                template ctx ns ("STM32DEV/" <> pName <> ".hs")

    forM_ [ RCC, FLASH, PWR ] genPeriph

    forM_ supportedPeriphs $ \p -> do

      -- per mcu peripheral definitions (mkXYZ)
      -- Ivory.BSP.STM32.{{ peripheral }}
      -- XXX: handle UART vs USART?
      case filter (((periph2svdName p)`L.isPrefixOf`) . periphName) $ devicePeripherals dev of
        [] -> fail $ "No peripheral found with groupName " ++ show p ++ " for device " ++ (deviceName dev)
        xs -> do
          print $ map periphName xs
          let ns = T.intercalate "." [ "STM32" <> T.pack namePart, "UART"]
              ctx = [
                  ("dev", lit $ T.pack namePart)
                , ("fam", lit $ T.take 2 $ T.pack namePart) -- XXX: < mcuFamily
                , ("version", lit "1") -- XXX
                ]
          template ctx ns "STM32DEV/UART.hs"

-- generate peripheral definitions (src/Ivory/BSP/STM32/Peripheral/
-- for all supportedPeriphs
stm32periphs DB{..} = do
  -- base non-versioned peripherals on this devices svd
  let nonVersionedBase = "STM32F765"
      nVDev = get nonVersionedBase
      get = getSVD svds

  forM_ supportedPeriphs $ \p -> do
    case filter ((==periph2svdName p) . periphGroupName) $ devicePeripherals nVDev of
      [] -> fail $ "No peripheral found with groupName " ++ show p ++ " for device " ++ (deviceName nVDev)
      x:xs -> do

        case versioned p of
          False -> do
             -- register bitdata
            let new = procPeriph p Nothing x
                res = ppPeriphRegs new
                ns = "STM32.Peripheral." <> tshow p <> ".Regs"
                ctx = [ ("regs", lit $ fixAndPack res)
                      , ("imports", lit "") ]
                      -- empty for CAN
                      -- "import STM32.Peripheral." <> tshow p <> ".RegTypes" ) ]
            template ctx ns "STM32/Peripheral/X/Regs.hs"

            let newP = fix4PeripheralDefinition new
            -- peripheral definition
            let ns = "STM32.Peripheral." <> tshow p <> ".Peripheral"
                ctx = [ ("type", lit $ tshow p)
                      , ("bitDataRegs", lit $ fixAndPack $ ppBitDataRegs newP)
                      , ("bitDataRegsMk", lit $ fixAndPack $ ppBitDataRegsMk newP)
                      ]
            template ctx ns ("STM32/Peripheral/" <> tshow p <> "/Peripheral.hs")


            -- reexports
            template [ ("type", lit $ tshow p) ]
              ("STM32.Peripheral." <> tshow p)
              ("STM32/Peripheral/X.hs")

          -- take a representative
          -- and build a DRIVER${version} dir out of it
          True -> do
            forM_ (filter (\(p', _, _) -> p' == p) representatives) $ \(_, ver, repre) -> do
              -- get representative for this peripheral version
              let x' = head . filter ((==periph2svdName p) . periphGroupName) $ devicePeripherals (get repre)

              -- register bitdata
              let new = procPeriph p (Just ver) x'
                  res = ppPeriphRegs new
                  ns = "STM32.Peripheral." <> tshow p <> tshow ver <> ".Regs"
                  ctx = [ ("regs", lit $ fixAndPack res)
                        , ("imports", lit $ "import Ivory.BSP.STM32.Peripheral." <> tshow p <> versionedRegTypes p ver <> ".RegTypes" ) ]
              template ctx ns "STM32/Peripheral/X/Regs.hs"

              let
                  common = [ "Regs" ]
                  commonForAllVersions = [
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
                  template [] ("STM32.Peripheral." <> (tshow p) <> "." <> file)
                              ("STM32/Peripheral/" <> (tshow p) <> "/" <> file <> ".hs")

              -- take stuff from UART1/file to UART1/file
              case (filter ((==p) . fst) versionSpecific) of
                [] -> return ()
                [(_, files)] -> forM_ files $ \file -> do
                  template [] ("STM32.Peripheral." <> (tshow p <> tshow ver) <> "." <> file)
                              ("STM32/Peripheral/" <> (tshow p <> tshow ver) <> "/" <> file <> ".hs")

               -- peripheral definition
              let ns = "STM32.Peripheral." <> (tshow p <> tshow ver) <> ".Peripheral"
                  ctx = [ ("type", lit $ tshow p)
                        , ("bitDataRegs", lit $ fixAndPack $ ppBitDataRegs new)
                        , ("bitDataRegsMk", lit $ fixAndPack $ ppBitDataRegsMk new)
                        , ("version", lit $ tshow ver)
                        ]
              template ctx ns ("STM32/Peripheral/" <> (tshow p <> tshow ver) <> "/Peripheral.hs")
            -- /forM for periph representatives

              case p of
                GPIO -> do
                      -- reexports
                      let ns = "STM32.Peripheral." <> tshow p
                          ctx =  [ ("type", lit (tshow p)) ]
                      template ctx ns "STM32/Peripheral/GPIO.hs"

                UART -> do
                      -- reexports
                      let ns = "STM32.Peripheral.UART"
                          ctx =  [ ("versions", litObj [ "1",  "2", "3"]) ]

                      template ctx ns "STM32/Peripheral/UART.hs"

                _    -> do
                      -- reexports
                      let ns = "STM32.Peripheral." <> tshow p
                          ctx =  [ ("type", lit $ (tshow p) <> (tshow ver)) ]
                      template ctx ns "STM32/Peripheral/X.hs"


  -- RCC reg types common for all devs
  template []
    "STM32.Peripheral.RCC.RegTypes"
    "STM32/Peripheral/RCC/RegTypes.hs"

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
  , (UART, 2, "F469")
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

-- Special driver/peripheral regs versioning treatment
--
-- UART needs UART1 & UARTv2 for old/new periph regs, has common driver
-- USART -> UART
-- SPI has common regs but two drivers
-- CAN needs special treatment for filters, bitArrays and composed FIFOs

filterRegsByName f x = x { periphRegisters = filter (f . regName) $ periphRegisters x }

-- common STM32 support (src/Ivory/BSP/STM32 and src/Ivory/BSP/ARMv7M)
stm32toplevel _db = do
  m <- genMCUData "devices.data"
  -- MCU
  let ctx = [ ("devices", lit $ T.pack m) ]
  template ctx "STM32.MCU" "STM32/MCU.hs"

  -- VectorTable
  let ctx = [ ("fams", litList $ map tshow supportedFamilies) ]
  template ctx "STM32.VectorTable" "STM32/VectorTable.hs"

  -- ClockInit
  let ctx = [ ("fams", litList $ map tshow supportedFamilies) ]
  template ctx "STM32.ClockInit" "STM32/ClockInit.hs"

  -- copied verbatim
  let copies = [
          "Core"
        , "ClockConfig"
        , "Family"
        , "Interrupt"
        , "LinkerScript"
        ]

  mapM_ (\x -> template [] ("STM32." <> x) $ T.concat ["STM32/", x, ".hs"]) copies

  cptree "../templates/ARMv7M/" "./src/Ivory/BSP/ARMv7M/"

template :: [(Text, Value)] -> Text -> Text -> IO ()
template context namespace tmpl = do
  t <- getTemplate tmpl
  let uglyFix = "\n" <> t -- work around karver not able to deal with {- at start of the file
  writeHS ns $ T.drop 1 $ renderTemplate (H.fromList ctx) uglyFix
  where
    ns = "Ivory.BSP." <> namespace
    ctx = ("modns" , lit ns):context

-- writeHS "STM32.Bla" "content"
writeHS :: Text -> Text -> IO ()
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

-- XXX breaks things when there are interrupt names ending with _
renameDups xs = reverse $ snd $ foldl f (S.empty, []) xs
  where f (seen, result) item | S.member (interruptName item) seen = f (seen, result) (item { interruptName = (interruptName item) ++ "_" })
                              | otherwise                          = (S.insert (interruptName item) seen, item:result)

svdsFamily :: DB -> Family -> [Device]
svdsFamily DB{..} f = map (getSVD svds . mcuRefName) $ fromJust $ M.lookup f cmxsWithSVD

stm32families db = do
  forM supportedFamilies $ \f -> do
    putStrLn $ "Processing family " ++ (show f)
    putStrLn $ let svdSet = S.fromList $ svdsFamily db f in
         "Distinct SVDs for this family "
      ++ (show . S.size $ svdSet)
      ++ " "
      ++ (show . S.map deviceName $ svdSet)
    let
      isr = replaceOne "|" "="
          $ T.pack
          $ ppISRs
          $ normalizeISRNames
          $ renameDups
          $ isrs
          $ svdsFamily db f

    template [ ("isr", lit isr) ]
      ("STM32" <> tshow f <> ".Interrupt")
       "STM32XX/Interrupt.hs"

    template [ ("isr", lit isr) ]
      ("STM32" <> tshow f <> ".ClockInit")
       "STM32XX/ClockInit.hs"
