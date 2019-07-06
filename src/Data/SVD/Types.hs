{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.SVD.Types where

import Data.Ord
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import GHC.Generics
import Data.Serialize
import Data.Algorithm.Diff

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
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Device

data Peripheral = Peripheral {
    periphName         :: String
  , periphDescription  :: String
  , periphDerivedFrom  :: Maybe String
  , periphGroupName    :: String
  , periphBaseAddress  :: Int
  , periphAddressBlock :: Maybe AddressBlock
  , periphInterrupts   :: [Interrupt]
  , periphRegisters    :: [Register]
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Peripheral

data AddressBlock = AddressBlock {
    addressBlockOffset :: Int
  , addressBlockSize   :: Int
  , addressBlockUsage  :: String
  } deriving (Generic, Eq, Ord, Show)

instance Serialize AddressBlock

data Interrupt = Interrupt {
    interruptName        :: String
  , interruptDescription :: String
  , interruptValue       :: Int
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Interrupt

data Register = Register {
    regName          :: String
  , regDisplayName   :: String
  , regDescription   :: String
  , regAddressOffset :: Int
  , regSize          :: Int
  , regAccess        :: AccessType
  , regResetValue    :: Int
  , regFields        :: [Field]
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Register

data AccessType = ReadOnly | WriteOnly | ReadWrite | WriteOnce | ReadWriteOnce
  deriving (Generic, Eq, Ord, Show)

instance Serialize AccessType

data Field = Field {
    fieldName        :: String
  , fieldDescription :: String
  , fieldBitOffset   :: Int
  , fieldBitWidth    :: Int
  , fieldReserved    :: Bool  -- so we can add reserved fields to the list
  , fieldRegType     :: Maybe String  -- ivory register type
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Field

toAccessType "read-only" = ReadOnly
toAccessType "write-only" = WriteOnly
toAccessType "read-write" = ReadWrite
toAccessType "writeOnce" = WriteOnce
toAccessType "read-writeOnce" = ReadWriteOnce
toAccessType x = error $ "Unable to read AccessType" ++ x

-- |Find holes in registers and create corresponding reserved fields for these
--
-- First finds missing missing bits and then merges them to single reserved field
procFields Register{..} = reverse $ sortByOffset (regFields ++ missingAsReserved)
  where
    missingAsReserved = reserved $ conts $ Set.toList missing

    reserved = map (\(offset, width) -> Field "_" "(Reserved)" offset width True Nothing)

    conts x = case cont x of
      [] -> []
      s -> [(head s, length s)] ++ conts (drop (length s) x)

    missing = all `Set.difference` existing

    all = Set.fromList [0..(regSize - 1)]

    existing = Set.fromList $ flip concatMap (sortByOffset regFields) $
      \Field{..} -> [fieldBitOffset .. (fieldBitOffset + fieldBitWidth - 1)]

    sortByOffset = sortOn fieldBitOffset

-- find longest increasing sequence
cont (x:y:xs) | (x + 1 == y) = [x] ++ cont (y:xs)
cont (x:xs)  = [x]
cont [] = []

-- walk processed register fields top to bottom
-- checking that the register is exactly n bits long
continuityCheck Register{..} = go regFields regSize
  where
  go [] 0 = True
  go (x:xs) remainingBits | fieldBitOffset x + fieldBitWidth x == remainingBits = go xs (remainingBits - fieldBitWidth x)
  go _ _                  | otherwise = False

mapPeriphs f Device{..} = map f devicePeripherals
mapRegs f Peripheral{..} = map f periphRegisters
mapFields f Register{..} = map f regFields

mapDevFields f d = concat $ concat $ flip mapPeriphs d $ mapRegs $ mapFields f

diffPeriphs a b = diffRegs (periphRegisters a) (periphRegisters b)

diffRegs a b = getDiffBy (\x y -> regName x == regName y) (sortOn regName a) (sortOn regName b)

-- |Get peripheral by name
getPeriph :: String -> Device -> Peripheral
getPeriph name dev = head . filter ((==name) . periphGroupName) $ devicePeripherals dev

getReg rName name dev = head . filter((==rName) . regName) . periphRegisters $ getPeriph name dev

registerNames pName dev = map regName . periphRegisters $ getPeriph pName dev
fieldNames rName pName dev = map fieldName . regFields $ getReg rName pName dev

diffRegisterNames pName dev1 dev2 = getDiff (registerNames pName dev1) (registerNames pName dev2)
diffFieldNames rName pName dev1 dev2 = getDiff (fieldNames rName pName dev1) (fieldNames rName pName dev2)

diffDistance x = sum $ map go x
  where
    go (Both _ _) = 0
    go (First  _) = 1
    go (Second _) = 1
