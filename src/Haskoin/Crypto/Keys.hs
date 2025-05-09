-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Crypto.Keys
  ( PublicKey (..),
    PrivateKey (..),
    wrapPubKey,
    derivePublicKey,
    wrapSecKey,
    fromWif,
    toWif,
  )
where

import Control.Monad (guard)
import Crypto.Secp256k1
  ( Ctx,
    PubKey,
    SecKey (..),
    derivePubKey,
    exportPubKey,
    importPubKey,
    secKey,
  )
import Data.Binary (Binary (..), getWord8)
import Data.Binary.Get (getByteString, lookAhead)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS
import Haskoin.Address.Base58 (Base58, decodeBase58Check, encodeBase58Check)
import Haskoin.Network (Network (secretPrefix))
import Haskoin.Util.Marshal (Marshal (..))

-- Elliptic curve public key type with expected serialized compression flag.
data PublicKey = PublicKey
  { point :: !PubKey,
    compress :: !Bool
  }
  deriving (Eq, Show)

-- Elliptic curve private key type with expected public key compression
-- information. Compression information is stored in private key WIF formats and
-- needs to be preserved to generate the correct address from the corresponding
-- public key.
data PrivateKey = PrivateKey
  { key :: !SecKey,
    compress :: !Bool
  }
  deriving (Eq, Show)

instance Marshal Ctx PublicKey where
  marshalGet ctx = do
    c <-
      lookAhead $
        getWord8 >>= \case
          0x02 -> return True
          0x03 -> return True
          0x04 -> return False
          _ -> fail "Not a public key"
    bs <- getByteString $ if c then 33 else 65
    case importPubKey ctx bs of
      Nothing -> fail "Could not decode public key"
      Just k -> return $ PublicKey k c

  marshalPut ctx pk =
    putByteString $ exportPubKey ctx pk.compress pk.point

instance Binary PrivateKey where
  put p = do
    _ <- putByteString p.key.get
    put p.compress
  get = do
    k <- getByteString 32
    c <- get
    return PrivateKey {key = SecKey k, compress = c}

-- Wrap a public key from secp256k1 library adding information about
-- compression.
wrapPubKey :: Bool -> PubKey -> PublicKey
wrapPubKey c p = PublicKey p c

-- Derives a public key from a private key. This function will preserve
-- compression flag.
derivePublicKey :: Ctx -> PrivateKey -> PublicKey
derivePublicKey ctx (PrivateKey d c) = PublicKey (derivePubKey ctx d) c

-- Wrap private key with corresponding public key compression flag.
wrapSecKey :: Bool -> SecKey -> PrivateKey
wrapSecKey c d = PrivateKey d c

-- Decode private key from WIF (wallet import format) string.
fromWif :: Network -> Base58 -> Maybe PrivateKey
fromWif net wif = do
  bs <- decodeBase58Check wif
  guard (BS.head bs == net.secretPrefix)
  case BS.length bs of
    33 -> wrapSecKey False <$> (secKey . BS.tail) bs
    34 -> do
      guard $ BS.last bs == 0x01
      wrapSecKey True <$> (secKey . BS.tail . BS.init) bs
    _ -> Nothing

-- Encode private key into a WIF string.
toWif :: Network -> PrivateKey -> Base58
toWif net (PrivateKey k c) =
  encodeBase58Check . BS.cons net.secretPrefix $
    if c then k.get `BS.snoc` 0x01 else k.get
