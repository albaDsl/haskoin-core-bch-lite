-- Copyright   : No rights reserved
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX

module Haskoin.Network (mainnet, chipnet, Network (..)) where

import Data.Text (Text)
import Data.Word (Word8)

data Network = Network
  { name :: !String,
    secretPrefix :: !Word8,
    cashAddrPrefix :: !(Maybe Text)
  }
  deriving (Eq, Show)

mainnet :: Network
mainnet =
  Network
    { name = "mainnet",
      secretPrefix = 128,
      cashAddrPrefix = Just "bitcoincash"
    }

chipnet :: Network
chipnet =
  Network
    { name = "chipnet",
      secretPrefix = 239,
      cashAddrPrefix = Just "bchtest"
    }
