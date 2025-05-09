-- Module      : Haskoin.Crypto.Signature
-- Copyright   : No rights reserved
-- License     : MIT
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : POSIX
{-# OPTIONS_GHC -Wno-orphans #-}

module Haskoin.Crypto.Signature
  ( signHash,
    verifyHashSig,
  )
where

import Control.Monad (guard, unless, when)
import Crypto.Secp256k1
  ( CompactSig (get),
    Ctx,
    Msg,
    PubKey,
    SecKey,
    Sig,
    exportCompactSig,
    exportSig,
    importSig,
    msg,
    normalizeSig,
    signMsg,
    verifySig,
  )
import Data.Binary (encode, getWord8)
import Data.Binary.Get (getByteString, lookAhead)
import Data.Binary.Put (putByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe, isNothing)
import Haskoin.Crypto.Hash (Hash256)
import Haskoin.Util.Marshal (Marshal (..))
import Numeric (showHex)

-- Sign a 256-bit hash using secp256k1 elliptic curve.
signHash :: Ctx -> SecKey -> Hash256 -> Sig
signHash ctx k = signMsg ctx k . hashToMsg

-- Verify an ECDSA signature for a 256-bit hash.
verifyHashSig :: Ctx -> Hash256 -> Sig -> PubKey -> Bool
verifyHashSig ctx h s p = verifySig ctx p norm (hashToMsg h)
  where
    norm = fromMaybe s (normalizeSig ctx s)

-- Convert 256-bit hash into a 'Msg' for signing or verification.
hashToMsg :: Hash256 -> Msg
hashToMsg =
  fromMaybe e . msg . toStrict . encode
  where
    e = error "Could not convert 32-byte hash to secp256k1 message"

instance Marshal Ctx Sig where
  marshalGet ctx = do
    l <- lookAhead $ do
      t <- getWord8
      -- 0x30 is DER sequence type
      unless (t == 0x30) $
        fail $
          "Bad DER identifier byte 0x" ++ showHex t ". Expecting 0x30"
      l <- getWord8
      when (l == 0x00) $ fail "Indeterminate form unsupported"
      when (l >= 0x80) $ fail "Multi-octect length not supported"
      return $ fromIntegral l
    bs <- getByteString $ l + 2
    case decodeStrictSig ctx bs of
      Just s -> return s
      Nothing -> fail "Invalid signature"

  marshalPut ctx s = putByteString $ exportSig ctx s

decodeStrictSig :: Ctx -> ByteString -> Maybe Sig
decodeStrictSig ctx bs = do
  g <- importSig ctx bs
  -- <http://www.secg.org/sec1-v2.pdf Section 4.1.4>
  -- 4.1.4.1 (r and s can not be zero)
  let compact = exportCompactSig ctx g
  let zero = B.replicate 32 0
  guard $ B.take 32 compact.get /= zero
  guard $ (B.take 32 . B.drop 32) compact.get /= zero
  guard $ isCanonicalHalfOrder ctx g
  return g

isCanonicalHalfOrder :: Ctx -> Sig -> Bool
isCanonicalHalfOrder ctx = isNothing . normalizeSig ctx
