{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.EffectiveSpread (EffectiveSpread(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data EffectiveSpread = EffectiveSpread {
  volume :: Integer,
  venue :: String,
  venueName :: String,
  effectiveSpread :: Double,
  effectiveQuoted :: Double,
  priceImprovement :: Double
} deriving (Generic, Show, Eq)

instance ToJSON EffectiveSpread
instance FromJSON EffectiveSpread
