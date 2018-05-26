{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.RecentStats (RecentStats(..)) where

import Data.Either
import Data.Maybe
import Data.Aeson
import GHC.Generics

data RecentStats = RecentStats {
  date :: String,
  volume :: Integer,
  routedVolume :: Integer,
  marketShare :: Double,
  isHalfday :: Bool,
  litVolume :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON RecentStats
instance FromJSON RecentStats
