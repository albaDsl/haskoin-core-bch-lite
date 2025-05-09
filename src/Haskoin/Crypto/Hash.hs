-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Crypto.Hash
  ( Hash256 (..),
    Hash160 (get),
    CheckSum32 (get),
    hash256,
    hash160,
    checkSum32,
  )
where

import Crypto.Hash (RIPEMD160 (RIPEMD160), SHA256 (..), hashWith)
import Data.Binary (Binary (..), decode)
import Data.Binary.Get (getByteString, getWord32be)
import Data.Binary.Put (putByteString, putWord32be)
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.String (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Word (Word32)
import Haskoin.Util.Helpers (decodeHex, encodeHex)

-- 'Word32' wrapped for type-safe 32-bit checksums.
newtype CheckSum32 = CheckSum32 {get :: Word32}
  deriving (Eq, Ord, Show)

newtype Hash256 = Hash256 {get :: ShortByteString}
  deriving (Eq, Ord)

newtype Hash160 = Hash160 {get :: ShortByteString}
  deriving (Eq, Ord)

instance Binary CheckSum32 where
  put (CheckSum32 c) = putWord32be c
  get = CheckSum32 <$> getWord32be

instance Show Hash256 where
  showsPrec _ = shows . encodeHex . fromShort . (.get)

instance Show Hash160 where
  showsPrec _ = shows . encodeHex . fromShort . (.get)

instance IsString Hash256 where
  fromString str =
    case decodeHex $ cs str of
      Nothing -> e
      Just bs ->
        case B.length bs of
          32 -> Hash256 (toShort bs)
          _ -> e
    where
      e = error "Could not decode hash from hex string"

instance Binary Hash256 where
  get = Hash256 . toShort <$> getByteString 32
  put = putByteString . fromShort . (.get)

instance IsString Hash160 where
  fromString str =
    case decodeHex $ cs str of
      Nothing -> e
      Just bs ->
        case B.length bs of
          20 -> Hash160 (toShort bs)
          _ -> e
    where
      e = error "Could not decode hash from hex string"

instance Binary Hash160 where
  get = Hash160 . toShort <$> getByteString 20
  put = putByteString . fromShort . (.get)

hash256 :: (ByteArrayAccess b) => b -> Hash256
hash256 = Hash256 . toShort . convert . hashWith SHA256 . hashWith SHA256

hash160 :: (ByteArrayAccess b) => b -> Hash160
hash160 = Hash160 . toShort . convert . hashWith RIPEMD160 . hashWith SHA256

checkSum32 :: (ByteArrayAccess b) => b -> CheckSum32
checkSum32 =
  decode
    . fromStrict
    . B.take 4
    . convert
    . hashWith SHA256
    . hashWith SHA256
