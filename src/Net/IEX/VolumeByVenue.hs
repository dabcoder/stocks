{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.VolumeByVenue (VolumeByVenue(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data VolumeByVenue = VolumeByVenue {
  volume :: Integer,
  venue :: String,
  venueName :: String,
  date :: Maybe String,
  marketPercent :: Double,
  avgMarketPercent :: Double
} deriving (Generic, Show, Eq)

instance ToJSON VolumeByVenue
instance FromJSON VolumeByVenue
