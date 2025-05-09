-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Transaction.SigHash
  ( SigHashType (..),
    SigHashFlag (..),
    sigHashAll,
    sigHashNone,
    sigHashSingle,
    anyoneCanPay,
    hasUtxosFlag,
    hasForkIdFlag,
    setAnyoneCanPay,
    setForkIdFlag,
    isSigHashAll,
    isSigHashNone,
    isSigHashSingle,
    isSigHashUnknown,
    sigHashAddForkId,
    sigHashGetForkId,
    TxSignature (..),
  )
where

import Control.Monad (when)
import Crypto.Secp256k1 (Ctx, Sig)
import Data.Binary.Get (getWord8, isEmpty)
import Data.Binary.Put (putWord8)
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.Bool (bool)
import Data.Word (Word32)
import Haskoin.Crypto.Signature ()
import Haskoin.Util.Marshal (Marshal (..))

data TxSignature
  = TxSignature
      { sig :: !Sig,
        hash :: !SigHashType
      }
  | TxSignatureEmpty
  deriving (Eq, Show)

newtype SigHashType = SigHashType Word32
  deriving (Eq, Ord, Enum, Show)
  deriving newtype (Bits, Integral, Num, Real)

data SigHashFlag
  = SIGHASH_ALL
  | SIGHASH_NONE
  | SIGHASH_SINGLE
  | SIGHASH_UTXOS
  | SIGHASH_FORKID
  | SIGHASH_ANYONECANPAY
  deriving (Eq, Ord, Show)

instance Marshal Ctx TxSignature where
  marshalPut _ctx TxSignatureEmpty = return ()
  marshalPut ctx (TxSignature sig (SigHashType n)) = do
    marshalPut ctx sig
    putWord8 (fromIntegral n)

  marshalGet ctx =
    bool decode empty =<< isEmpty
    where
      empty = return TxSignatureEmpty
      decode = do
        sig <- marshalGet ctx
        sh <- fromIntegral <$> getWord8
        when (isSigHashUnknown sh) $
          fail "Non-canonical signature: unknown hashtype byte"
        return $ TxSignature sig sh

instance Enum SigHashFlag where
  fromEnum SIGHASH_ALL = 0x01
  fromEnum SIGHASH_NONE = 0x02
  fromEnum SIGHASH_SINGLE = 0x03
  fromEnum SIGHASH_UTXOS = 0x20
  fromEnum SIGHASH_FORKID = 0x40
  fromEnum SIGHASH_ANYONECANPAY = 0x80
  toEnum 0x01 = SIGHASH_ALL
  toEnum 0x02 = SIGHASH_NONE
  toEnum 0x03 = SIGHASH_SINGLE
  toEnum 0x20 = SIGHASH_UTXOS
  toEnum 0x40 = SIGHASH_FORKID
  toEnum 0x80 = SIGHASH_ANYONECANPAY
  toEnum _ = error "Not a valid sighash flag"

sigHashNone :: SigHashType
sigHashNone = fromIntegral $ fromEnum SIGHASH_NONE

sigHashAll :: SigHashType
sigHashAll = fromIntegral $ fromEnum SIGHASH_ALL

sigHashSingle :: SigHashType
sigHashSingle = fromIntegral $ fromEnum SIGHASH_SINGLE

sigHashUtxos :: SigHashType
sigHashUtxos = fromIntegral $ fromEnum SIGHASH_UTXOS

sigHashForkId :: SigHashType
sigHashForkId = fromIntegral $ fromEnum SIGHASH_FORKID

sigHashAnyoneCanPay :: SigHashType
sigHashAnyoneCanPay = fromIntegral $ fromEnum SIGHASH_ANYONECANPAY

setForkIdFlag :: SigHashType -> SigHashType
setForkIdFlag = (.|. sigHashForkId)

setAnyoneCanPay :: SigHashType -> SigHashType
setAnyoneCanPay = (.|. sigHashAnyoneCanPay)

hasUtxosFlag :: SigHashType -> Bool
hasUtxosFlag = (/= 0) . (.&. sigHashUtxos)

hasForkIdFlag :: SigHashType -> Bool
hasForkIdFlag = (/= 0) . (.&. sigHashForkId)

anyoneCanPay :: SigHashType -> Bool
anyoneCanPay = (/= 0) . (.&. sigHashAnyoneCanPay)

isSigHashAll :: SigHashType -> Bool
isSigHashAll = (== sigHashAll) . (.&. 0x1f)

isSigHashNone :: SigHashType -> Bool
isSigHashNone = (== sigHashNone) . (.&. 0x1f)

isSigHashSingle :: SigHashType -> Bool
isSigHashSingle = (== sigHashSingle) . (.&. 0x1f)

isSigHashUnknown :: SigHashType -> Bool
isSigHashUnknown =
  (`notElem` [sigHashAll, sigHashNone, sigHashSingle]) . (.&. 0x1f)

sigHashAddForkId :: SigHashType -> Word32 -> SigHashType
sigHashAddForkId sh w = (fromIntegral w `shiftL` 8) .|. (sh .&. 0x000000ff)

sigHashGetForkId :: SigHashType -> Word32
sigHashGetForkId (SigHashType n) = fromIntegral $ n `shiftR` 8
