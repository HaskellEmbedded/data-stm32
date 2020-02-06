{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.SVD.Types where

import Data.Bits
import Data.Char (toLower)
import Data.Ord
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Safe

import Control.Monad

import GHC.Generics
import Data.Serialize
import Data.Algorithm.Diff
import Data.Ivory.Pretty (hexFormat)

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
  , regResetValue    :: Maybe Int
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

toAccessType :: String -> AccessType
toAccessType "read-only"      = ReadOnly
toAccessType "write-only"     = WriteOnly
toAccessType "read-write"     = ReadWrite
toAccessType "writeOnce"      = WriteOnce
toAccessType "read-writeOnce" = ReadWriteOnce
toAccessType x                = error $ "Unable to read AccessType" ++ x

showAccessType :: AccessType -> String
showAccessType ReadOnly       = "read-only"
showAccessType WriteOnly      = "write-only"
showAccessType ReadWrite      = "read-write"
showAccessType WriteOnce      = "writeOnce"
showAccessType ReadWriteOnce  = "read-writeOnce"


-- |Find holes in registers and create corresponding reserved fields for these
--
-- First finds missing missing bits and then merges them to single reserved field
procFields Register{..} = dataIfSingleReserved $ reverse $ sortByOffset (regFields ++ missingAsReserved)
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

    -- this handles a case when there are no fields and code above creates a single full-sized reserved field
    -- which we turn into non-reserved "data" field
    dataIfSingleReserved [f] | fieldReserved f == True = [ f { fieldName = "DATA", fieldReserved = False } ]
    dataIfSingleReserved fs  | otherwise             = fs

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

-- |Get peripheral by groupName
getPeriphByGroup :: String -> Device -> Peripheral
getPeriphByGroup name dev = headNote ("getPeriphByGroup " ++ name) . filterLowerBy name periphGroupName $ devicePeripherals dev

-- |Get peripheral by name
getPeriph :: String -> Device -> Peripheral
getPeriph name dev = headNote ("getPeriph " ++ name) . filterLowerBy name periphName $ devicePeripherals dev

-- |Get peripheral by name iff found, Nothing otherwise
getPeriphMay :: String -> Device -> Maybe Peripheral
getPeriphMay name dev = headMay . filterLowerBy name periphName $ devicePeripherals dev

-- |Get register of the peripheral by their names iff found, Nothing otherwise
getPeriphRegMay :: String -> Peripheral -> Maybe Register
getPeriphRegMay rName = headMay . filterLowerBy rName regName . periphRegisters

-- |Filter elements matching lowercased `eqTo` after applying `by`
filterLowerBy :: String -> (a -> String) -> [a] -> [a]
filterLowerBy eqTo by = filter $ (==(map toLower eqTo)) . map toLower . by

-- |Get peripheral by name or its parent peripheral if it's
-- a derived peripheral (for example USART2 is typically derived from USART1)
getPeriphFollow :: String -> Device -> Either String Peripheral
getPeriphFollow pName dev = case getPeriphMay pName dev of
  Nothing -> Left $ "No peripheral found: " ++ pName
  Just p  -> case periphDerivedFrom p of
    Nothing -> Right p
    Just fromName -> case getPeriphMay fromName dev of
      Nothing -> Left $ "Parent peripheral not found: " ++ fromName ++ " for peripheral " ++ pName
      Just parentPeriph -> Right $ parentPeriph

-- |Get registers of the peripheral
getPeriphRegs :: String -> Device -> Either String [Register]
getPeriphRegs pName dev = periphRegisters <$> getPeriphFollow pName dev

-- |Get specific register of the peripheral
getPeriphReg :: String -> String -> Device -> Either String Register
getPeriphReg pName rName dev = either Left (maybeToEither errMsg . getPeriphRegMay rName) (getPeriphFollow pName dev)
  where
    errMsg = "No register found: " ++ rName ++ " for peripheral " ++ pName
    maybeToEither msg m = case m of
      Just x -> Right x
      Nothing -> Left msg

-- |Get address of the specific register of the peripheral with `pName`
getPeriphRegAddr :: String -> String -> Device -> Either String Int
getPeriphRegAddr pName rName dev = (\p r -> periphBaseAddress p + regAddressOffset r) <$> getPeriphFollow pName dev <*> getPeriphReg pName rName dev

-- |Get fields of the specific register of the peripheral with `pName`
getPeriphRegFields :: String -> String -> Device -> Either String [Field]
getPeriphRegFields pName rName dev = regFields <$> getPeriphReg pName rName dev

-- old
getReg pName rName dev = headNote "getReg" . filter((==rName) . regName) . periphRegisters $ getPeriph pName dev
getRegFields pName rName dev = regFields $ getReg pName rName dev

-- |Get value of specific Field according to input `x`
getFieldVal :: (Bits a, Num a) => a -> Field -> a
getFieldVal x f = (x `shiftR` fieldBitOffset f) .&. (2 ^ fieldBitWidth f - 1)

-- |Decode integer `x` according to Fields `fs`
getFieldValues :: (Bits a, Num a) => a -> [Field] -> [(a, Field)]
getFieldValues x fs = zip (map (getFieldVal x) fs) fs

-- |Same as `getFieldValues` but with processed fields (reserved fields included)
getProcdFieldValues x fs = getFieldValues x $ procFields fs

-- |Check if any reserved field has value other than 0
anyReservedSet :: (Eq a, Num a) => [(a, Field)] -> Bool
anyReservedSet = any (\(val, f) -> val /= 0 && fieldReserved f)

-- |Filter fields with non zero value
filterSet :: (Eq a, Num a) => [(a, Field)] -> [(a, Field)]
filterSet = filter ((/= 0) . fst)

-- |Get memory map of the device according to its perhiperal addresses
getDevMemMap Device{..} = map (liftM2 (,) (hexFormat . periphBaseAddress) periphName) devicePeripherals

registerNames pName dev = map regName . periphRegisters $ getPeriph pName dev
fieldNames rName pName dev = map fieldName $ getRegFields pName rName dev

-- Diffing

diffPeriphNames dev1 dev2 = getDiff
  (sort $ map periphName $ devicePeripherals dev1)
  (sort $ map periphName $ devicePeripherals dev2)

diffRegisterNames pName dev1 dev2 = getDiff
  (sort $ registerNames pName dev1)
  (sort $ registerNames pName dev2)

regNames = map regName . periphRegisters
diffRegNames p1 p2 = diff regNames p1 p2

regNameFields rName p = regFields . headNote "regNameFields" . filter((==rName) . regName) . periphRegisters $ p

diff fn x y = getDiff (sort $ fn x) (sort $ fn y)

diffFieldNames pName regName dev1 dev2 = getDiff
  (sort $ fieldNames regName pName dev1)
  (sort $ fieldNames regName pName dev2)

cmps fn a b = fn a == fn b

diffFields as bs = getDiffBy (\x y ->
    cmps fieldName x y
    && cmps fieldBitWidth x y
    && cmps fieldBitOffset x y)
  (sortOn fieldBitOffset as)
  (sortOn fieldBitOffset bs)

diffDistance x = sum $ map go x
  where
    go (Both _ _) = 0
    go (First  _) = 1
    go (Second _) = 1

isBoth (Both _ _) = True
isBoth _ = False

getBoths = map (\(Both x _) -> x) . filter isBoth

defaultDevice = Device {
    deviceName            = "defaultDev"
  , deviceVersion         = mempty
  , deviceDescription     = mempty
  , deviceAddressUnitBits = 0
  , deviceWidth           = 0
  , deviceSize            = 0
  , deviceResetValue      = 0
  , deviceResetMask       = 0
  , devicePeripherals     = []
  }

defaultPeripheral = Peripheral {
    periphName         = "defaultPeriph"
  , periphDescription  = mempty
  , periphDerivedFrom  = Nothing
  , periphGroupName    = mempty
  , periphBaseAddress  = 0
  , periphAddressBlock = Nothing
  , periphInterrupts   = []
  , periphRegisters    = []
  }

defaultRegister = Register {
    regName          = "defaultRegister"
  , regDisplayName   = mempty
  , regDescription   = mempty
  , regAddressOffset = 0
  , regSize          = 0
  , regAccess        = ReadOnly
  , regResetValue    = Nothing
  , regFields        = []
  }

defaultField = Field {
    fieldName        = "defaultField"
  , fieldDescription = mempty
  , fieldBitOffset   = 0
  , fieldBitWidth    = 0
  , fieldReserved    = False
  , fieldRegType     = Nothing
  }
