-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Util.Helpers
  ( bsToInteger,
    integerToBS,
    encodeHex,
    decodeHex,
    convertBits,
  )
where

import Data.Base16.Types (assertBase16, extractBase16)
import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Base16 (decodeBase16, encodeBase16, isBase16)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word (Word8)

-- Decode a big endian 'Integer' from a 'ByteString'.
bsToInteger :: ByteString -> Integer
bsToInteger = B.foldr f 0 . B.reverse
  where
    f w n = toInteger w .|. shiftL n 8

-- Encode an 'Integer' to a 'ByteString' as big endian.
integerToBS :: Integer -> ByteString
integerToBS 0 = B.pack [0]
integerToBS i
  | i > 0 = B.reverse $ B.unfoldr f i
  | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)

encodeHex :: ByteString -> Text
encodeHex = extractBase16 . encodeBase16

decodeHex :: Text -> Maybe ByteString
decodeHex t =
  if isBase16 u8
    then Just . decodeBase16 $ assertBase16 u8
    else Nothing
  where
    u8 = T.encodeUtf8 t

-- Convert from one power-of-two base to another, as long as it fits in a
-- 'Word'.
convertBits :: Bool -> Int -> Int -> [Word] -> ([Word], Bool)
convertBits pad frombits tobits i = (reverse yout, rem')
  where
    (xacc, xbits, xout) = foldl' outer (0, 0, []) i
    (yout, rem')
      | pad && xbits /= 0 =
          let xout' = (xacc `shiftL` (tobits - xbits)) .&. maxv : xout
           in (xout', False)
      | pad = (xout, False)
      | xbits /= 0 = (xout, True)
      | otherwise = (xout, False)
    maxv = 1 `shiftL` tobits - 1
    max_acc = 1 `shiftL` (frombits + tobits - 1) - 1
    outer (acc, bits, out) it =
      let acc' = ((acc `shiftL` frombits) .|. it) .&. max_acc
          bits' = bits + frombits
          (out', bits'') = inner acc' out bits'
       in (acc', bits'', out')
    inner acc out bits
      | bits >= tobits =
          let bits' = bits - tobits
              out' = ((acc `shiftR` bits') .&. maxv) : out
           in inner acc out' bits'
      | otherwise = (out, bits)
