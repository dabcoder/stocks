{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.IntraDayStats (IntraDayStats(..),
                              IntraDaySub(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data IntraDayStats = IntraDayStats {
  volume :: IntraDaySub,
  symbolsTraded :: IntraDaySub,
  routedVolume :: IntraDaySub,
  notional :: IntraDaySub,
  marketShare :: IntraDaySub
} deriving (Generic, Show, Eq)

data IntraDaySub = IntraDaySub {
  value :: Maybe Double,
  lastUpdated :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON IntraDayStats
instance ToJSON IntraDaySub
instance FromJSON IntraDayStats
instance FromJSON IntraDaySub
