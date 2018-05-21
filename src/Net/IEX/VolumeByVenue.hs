{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.VolumeByVenue (VolumeByVenue(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
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
