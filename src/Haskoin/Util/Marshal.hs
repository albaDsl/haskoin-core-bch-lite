-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE FunctionalDependencies #-}

module Haskoin.Util.Marshal where

import Data.Binary (Get, Put)
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL

class Marshal s a | a -> s where
  marshalPut :: s -> a -> Put
  marshalGet :: s -> Get a

marshal :: (Marshal s a) => s -> a -> ByteString
marshal s = BL.toStrict . runPut . marshalPut s

unmarshal :: (Marshal s a) => s -> ByteString -> Either String a
unmarshal s b = do
  let b' = BL.fromStrict b
  case (runGetOrFail . marshalGet) s b' of
    Left (_, _, err) -> Left err
    Right (_, _, x) -> Right x
