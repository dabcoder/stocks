{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Previous (Previous(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data Previous = Previous {
  symbol :: String,
  date :: String,
  open :: Double,
  high :: Double,
  low :: Double,
  close :: Double,
  volume :: Integer,
  unadjustedVolume :: Integer,
  change :: Double,
  changePercent :: Double,
  vwap :: Double
} deriving (Generic, Show, Eq)

instance ToJSON Previous
instance FromJSON Previous
