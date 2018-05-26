{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.RecordStats (RecordStats(..),
                            RecordStatsSub(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data RecordStats = RecordStats {
  volume :: RecordStatsSub,
  symbolsTraded :: RecordStatsSub,
  routedVolume :: RecordStatsSub,
  notional :: RecordStatsSub
} deriving (Generic, Show, Eq)

data RecordStatsSub = RecordStatsSub {
  recordValue :: Double,
  recordDate :: String,
  previousDayValue :: Double,
  avg30Value :: Double
} deriving (Generic, Show, Eq)

instance ToJSON RecordStats
instance ToJSON RecordStatsSub
instance FromJSON RecordStats
instance FromJSON RecordStatsSub
