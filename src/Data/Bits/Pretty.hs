module Data.Bits.Pretty (
    showHex
  , showBin
  , showBinGroups
  , showDec
  , platformSizeOfInt
  , showHex8
  , showHex16
  , showHex32
  ) where

import Data.Int
import Data.Bits
import Data.Word
import Text.Printf

-- |Format number using hexadecimal notation with leading 0x, padded according to its bit size
showHex :: (PrintfArg t, FiniteBits t) => t -> String
showHex x = printf ("0x%0" ++ (show $ finiteBitSize x `div` 4) ++ "X") x

showBin :: (PrintfArg t, FiniteBits t) => t -> String
showBin x = printf ("0b%0" ++ (show $ finiteBitSize x) ++ "b") x

showDec :: (PrintfArg t, FiniteBits t) => t -> String
showDec x = printf ("%0"   ++ (show decSize) ++ "u") x
  where
    decSize :: Int
    decSize = ceiling $ logBase (10 :: Double) (2 ^ (finiteBitSize x))

-- |Print number in binary with bits grouped by `groupSize`
-- e.g. with `groupSize = 4` we would get `0b0000 1010 0000 0101`
showBinGroups :: (PrintfArg b, Num b, FiniteBits b) => Int -> b -> String
showBinGroups groupSize x = unwords $ flip map [gs, gs - 1 ..0] $ \g -> ((printf ("%0" ++ (show groupSize) ++ "b") (mask g x)) :: String)
  where
    mask g n = (2^groupSize - 1) .&. (n `shiftR` (fromIntegral (g * groupSize)))
    gs = sz `div` groupSize
    sz = fromIntegral $ finiteBitSize x

-- |Size of `Int` at current platform
platformSizeOfInt :: Int
platformSizeOfInt = finiteBitSize (0 :: Int)

-- shortcuts

showHex32 :: Int -> String
showHex32 = showHex . (fromIntegral :: Int -> Word32)

showHex16 :: Int -> String
showHex16 = showHex . (fromIntegral :: Int -> Word16)

showHex8 :: Int -> String
showHex8  = showHex . (fromIntegral :: Int -> Word8)
