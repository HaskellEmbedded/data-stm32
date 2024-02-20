module Data.STM32.Memory where

import Data.STM32.Types
import Data.CMX.Types
import qualified Data.Maybe

ramOffset :: NamedMCU -> Int
-- for f7 this is CCM + ramOffset
ramOffset (_name, mcu) | mcuFamily mcu == F7 = 0x20000000 + Data.Maybe.fromMaybe (error "F7 should have mcuCcmRam not Nothing") (mcuCcmRam mcu)
ramOffset _  = 0x20000000

-- for some we need to fill ccmSize as it's not part of svd files
ccmSize :: NamedMCU -> Maybe Int
ccmSize nmcu        | isFamDev F3 ["03"] nmcu =
  case stmFlashSizeId (fst nmcu) of
    Just '6' -> kb 4
    Just '8' -> kb 4
    Just 'B' -> kb 8
    Just 'C' -> kb 8
    Just 'D' -> kb 16
    Just 'E' -> kb 16
    _        -> Nothing
ccmSize nmcu        | isFamDev G4 ["31", "41"] nmcu = kb 10
ccmSize nmcu        | isFamDev G4 ["71"] nmcu = kb 16
ccmSize nmcu        | isFamDev G4 ["73", "74", "83", "84"] nmcu = kb 32
ccmSize _                       = Nothing

ccmOffset :: NamedMCU -> Maybe Int
ccmOffset (_name, mcu) = offset $ mcuFamily mcu
  where
    offset F3 = Just 0x10000000
    offset F4 = Just 0x10000000
    offset F7 = Just 0x20000000
    offset G4 = Just 0x10000000
    offset _  = Nothing

backupOffset :: NamedMCU -> Maybe Int
backupOffset (_name, mcu) = offset $ mcuFamily mcu
  where
    offset :: Family -> Maybe Int
    offset F4 = Just 0x40024000
    offset F7 = Just 0x40024000
    offset _  = Nothing

eepromOffset :: NamedMCU -> Maybe Int
eepromOffset (_name, mcu) = offset $ mcuFamily mcu
  where
    offset L0 = Just 0x08080000
    offset L1 = Just 0x08080000
    offset L4 = Just 0x08080000
    offset _  = Nothing

isFamDev :: Family -> [String] -> (STM32DevName, MCU) -> Bool
isFamDev fam names (devName, mcu) = mcuFamily mcu == fam
                                    && stmName devName `elem` names

-- sram2 is non-continuos for these STM32L475xx/476xx/486xx devices
l4nc:: (STM32DevName, MCU) -> Bool
l4nc = isFamDev L4 ["75", "76", "86"]

--ramContinuous = not . l4nc

ram2Offset :: NamedMCU -> Maybe Int
ram2Offset nmcu | l4nc nmcu = Just 0x10000000
-- ram2 follows ram1 if mcuRam2 is not Nothing
ram2Offset nmcu@(_name, mcu) = pure (Just (ramOffset nmcu + mcuRam1 mcu)) =<< mcuRam2 mcu

kb :: Int -> Maybe Int
kb = Just . (*1024)

ram2Size :: NamedMCU -> Maybe Int
ram2Size nmcu | (mcuFamily . snd $ nmcu) == F2 = kb 16
ram2Size nmcu | isFamDev F4 ["05", "07", "15", "17", "27", "29", "37", "39"] nmcu = kb 16
ram2Size nmcu | isFamDev F4 ["69", "79"] nmcu = kb 32
ram2Size nmcu | (mcuFamily . snd $ nmcu) == F7 = kb 16
ram2Size nmcu | isFamDev G4 ["31", "41", "71"] nmcu = kb 6
ram2Size nmcu | isFamDev G4 ["73", "74", "83", "84"] nmcu = kb 16
ram2Size nmcu | isFamDev L4 ["12", "22"] nmcu = kb 8
ram2Size nmcu | isFamDev L4 ["32", "42", "52", "62"] nmcu = kb 16
ram2Size nmcu | isFamDev L4 ["31", "33", "43", "51", "71", "75", "76", "85", "86", "96", "a6"] nmcu = kb 32
ram2Size nmcu | (mcuFamily . snd $ nmcu) == L4Plus = kb 64
ram2Size _ = Nothing

ram3Offset :: NamedMCU -> Maybe Int
-- redundat to bellow case? ram3Offset (name, mcu) | mcuFamily mcu == L4 = Just 0x20040000
-- ram3 follows ram1,ram2
ram3Offset nmcu@(_name, mcu) = pure (Just (ramOffset nmcu + mcuRam1 mcu + Data.Maybe.fromMaybe 0 (mcuRam2 mcu))) =<< mcuRam3 mcu

ram3Size :: NamedMCU -> Maybe Int
ram3Size nmcu | (mcuFamily . snd $ nmcu) == L4Plus = kb 384
ram3Size nmcu | isFamDev F4 ["27", "29", "37", "39"] nmcu = kb 64
ram3Size nmcu | isFamDev F4 ["69", "79"] nmcu = kb 128
ram3Size _ = Nothing

rams :: (STM32DevName, MCU) -> [(Int, Int)]
rams nmcu@(_name, mcu) =
  [(ramOffset nmcu, mcuRam1 mcu)]
  ++ maybe [] (\x -> [(Data.Maybe.fromMaybe (error "has mcuRam2 but no ram2Offset") $ ram2Offset nmcu, x)]) (mcuRam2 mcu)
  ++ maybe [] (\x -> [(Data.Maybe.fromMaybe (error "has mcuRam3 but no ram3Offset") $ ram3Offset nmcu, x)]) (mcuRam3 mcu)

ramContinuos :: [(Int, Int)] -> Bool
ramContinuos [] = True
ramContinuos ((fstOffset, fstSize):mems) = conts (fstOffset+fstSize) mems
  where
  conts coffs ((offs, size):xs) | offs == coffs = conts (offs+size) xs
  conts _ [] = True
  conts _ _ = False

periphBase :: Int
periphBase = 0x40000000

flashOffset :: Integer
flashOffset = 0x08000000


-- unused for now
-- info  base 0x1ffff000

bases :: Family -> [(String, Int)]
bases F0 =
  [ ("apb1", periphBase)
  , ("ahb1", periphBase + 0x20000)
  , ("ahb2", periphBase + 0x08000000)
  ]

bases F1 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x18000)
  ]

bases F2 = bases F7
bases F3 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x20000)
  , ("ahb2", 0x48000000)
  , ("ahb3", 0x50000000)
  ]

bases F4 = bases F7
bases F7 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x20000)
  , ("ahb2", 0x50000000)
  , ("ahb3", 0x60000000)
  ]

bases L0 =
  [ ("apb1", periphBase)
  , ("apb2", periphBase + 0x10000)
  , ("ahb1", periphBase + 0x20000)
  ]
  -- ioport base 0x50000000

bases L1 = bases L0

bases L4 = bases L0 ++ [ ("ahb2", 0x48000000) ]
  -- fmc1 base 0x60000000
  -- fmc3 base 0x80000000
  -- qspi bank base 0x90000000
  -- fmc qspi base 0xA0000000
bases x = error $ "no bases info for " ++ show x
