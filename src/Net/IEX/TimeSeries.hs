{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.TimeSeries (TimeSeries(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data TimeSeries = TimeSeries {
  date :: String,
  open :: Double,
  high :: Double,
  low :: Double,
  close :: Double,
  volume :: Integer,
  unadjustedVolume :: Integer,
  change :: Double,
  changePercent :: Double,
  vwap :: Double,
  label :: String,
  changeOverTime :: Double
} deriving (Generic, Show, Eq)

instance ToJSON TimeSeries
instance FromJSON TimeSeries
