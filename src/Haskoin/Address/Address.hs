-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Address.Address
  ( Address (..),
    addrToText,
    textToAddr,
    pubKeyAddr,
    module Haskoin.Address.CashAddr,
  )
where

import Crypto.Secp256k1 (Ctx)
import Data.Binary (Binary (..), decode, encode)
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import Data.Text qualified as T
import Haskoin.Address.CashAddr (cashAddrDecode, cashAddrEncode)
import Haskoin.Crypto.Hash (Hash160, hash160)
import Haskoin.Crypto.Keys (PublicKey)
import Haskoin.Network (Network)
import Haskoin.Util.Helpers (encodeHex)
import Haskoin.Util.Marshal (marshal)

data Address
  = PubKeyAddress {hash160 :: !Hash160}
  | ScriptAddress {hash160 :: !Hash160}
  deriving (Eq, Show)

instance Binary Address where
  put (PubKeyAddress h) = do
    putWord8 0x00
    put h
  put (ScriptAddress h) = do
    putWord8 0x01
    put h

  get =
    getWord8 >>= \case
      0x00 -> PubKeyAddress <$> get
      0x01 -> ScriptAddress <$> get
      b ->
        fail . T.unpack $
          "Could not decode address type byte: "
            <> encodeHex (B.singleton b)

addrToText :: Network -> Address -> Maybe Text
addrToText net PubKeyAddress {hash160 = h} =
  cashAddrEncode net 0 (toStrict $ encode h)
addrToText net ScriptAddress {hash160 = h} =
  cashAddrEncode net 1 (toStrict $ encode h)

textToAddr :: Network -> Text -> Maybe Address
textToAddr net txt = do
  (ver, bs) <- cashAddrDecode net txt
  case ver of
    0 -> Just $ PubKeyAddress (decode (fromStrict bs))
    1 -> Just $ ScriptAddress (decode (fromStrict bs))
    _ -> Nothing

pubKeyAddr :: Ctx -> PublicKey -> Address
pubKeyAddr ctx = PubKeyAddress . hash160 . marshal ctx
