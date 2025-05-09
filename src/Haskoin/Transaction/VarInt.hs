-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Transaction.VarInt
  ( VarInt (..),
    putVarInt,
  )
where

import Data.Binary (Binary (..))
import Data.Binary.Get (getWord16le, getWord32le, getWord64le, getWord8)
import Data.Binary.Put (Put, putWord16le, putWord32le, putWord64le, putWord8)
import Data.Word (Word64)

newtype VarInt = VarInt {get :: Word64}
  deriving (Eq, Show)

instance Binary VarInt where
  get = VarInt <$> (getWord8 >>= go)
    where
      go 0xff = getWord64le
      go 0xfe = fromIntegral <$> getWord32le
      go 0xfd = fromIntegral <$> getWord16le
      go x = return $ fromIntegral x

  put (VarInt x)
    | x < 0xfd =
        putWord8 $ fromIntegral x
    | x <= 0xffff = do
        putWord8 0xfd
        putWord16le $ fromIntegral x
    | x <= 0xffffffff = do
        putWord8 0xfe
        putWord32le $ fromIntegral x
    | otherwise = do
        putWord8 0xff
        putWord64le x

putVarInt :: (Integral a) => a -> Put
putVarInt = put . VarInt . fromIntegral
