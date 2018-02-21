{-# LANGUAGE RecordWildCards #-}
module Data.SVD.Types where

import Data.List
import qualified Data.Set as Set


data Device = Device {
    deviceName            :: String
  , deviceVersion         :: String
  , deviceDescription     :: String
  , deviceAddressUnitBits :: Int
  , deviceWidth           :: Int
  , deviceSize            :: Int
  , deviceResetValue      :: Int
  , deviceResetMask       :: Int
  , devicePeripherals     :: [Peripheral]
  } deriving (Eq, Ord, Show)

data Peripheral = Peripheral {
    periphName         :: String
  , periphDescription  :: String
  , periphDerivedFrom  :: Maybe String
  , periphGroupName    :: String
  , periphBaseAddress  :: Int
  , periphAddressBlock :: Maybe AddressBlock
  , periphInterrupts   :: [Interrupt]
  , periphRegisters    :: [Register]
  } deriving (Eq, Ord, Show)

data AddressBlock = AddressBlock {
    addressBlockOffset :: Int
  , addressBlockSize   :: Int
  , addressBlockUsage  :: String
  } deriving (Eq, Ord, Show)

data Interrupt = Interrupt {
    interruptName        :: String
  , interruptDescription :: String
  , interruptValue       :: Int
  } deriving (Eq, Ord, Show)

data Register = Register {
    regName          :: String
  , regDisplayName   :: String
  , regDescription   :: String
  , regAddressOffset :: Int
  , regSize          :: Int
  , regAccess        :: AccessType
  , regResetValue    :: Int
  , regFields        :: [Field]
  } deriving (Eq, Ord, Show)

data AccessType = ReadOnly | WriteOnly | ReadWrite | WriteOnce | ReadWriteOnce
  deriving (Eq, Ord, Show)

data Field = Field {
    fieldName        :: String
  , fieldDescription :: String
  , fieldBitOffset   :: Int
  , fieldBitWidth    :: Int
  , fieldReserved    :: Bool  -- so we can add reserved fields to the list
  } deriving (Eq, Ord, Show)

toAccessType "read-only" = ReadOnly
toAccessType "write-only" = WriteOnly
toAccessType "read-write" = ReadWrite
toAccessType "writeOnce" = WriteOnce
toAccessType "read-writeOnce" = ReadWriteOnce
toAccessType x = error $ "Unable to read AccessType" ++ x

-- find holes in registers and create corresponding reserved fields for these
-- this first finds missing missing bits and them merges them to single reserved field
procFields f = reverse $ sortByOffset (f ++ missingAsReserved)
  where
    missingAsReserved = reserved $ conts $ Set.toList missing

    reserved = map (\(offset, width) -> Field "_" "Reserved" offset width True)

    conts x = case cont x of
      [] -> []
      s -> [(head s, length s)] ++ conts (drop (length s) x)

    missing = all `Set.difference` existing

    all = Set.fromList [0..31]

    existing = Set.fromList $ flip concatMap (sortByOffset f) $
      \Field{..} -> [fieldBitOffset .. (fieldBitOffset + fieldBitWidth - 1)]

    sortByOffset = sortOn fieldBitOffset

-- find longest increasing sequence
cont (x:y:xs) | (x + 1 == y) = [x] ++ cont (y:xs)
cont (x:xs)  = [x]
cont [] = []


mapPeriphs f Device{..} = map f devicePeripherals
mapRegs f Peripheral{..} = map f periphRegisters
mapFields f Register{..} = map f regFields

mapDevFields f d = concat $ concat $ flip mapPeriphs d $ mapRegs $ mapFields f


