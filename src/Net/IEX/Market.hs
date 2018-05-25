{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Market (Market(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data Market = Market {
  mic :: String,
  tapeId :: String,
  venueName :: String,
  volume :: Integer,
  tapeA :: Integer,
  tapeB :: Integer,
  tapeC :: Integer,
  marketPercent :: Double,
  lastUpdated :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON Market
instance FromJSON Market
