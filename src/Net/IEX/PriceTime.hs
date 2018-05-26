{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.PriceTime (PriceTime(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data PriceTime = PriceTime {
  price :: Double,
  time :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON PriceTime
instance FromJSON PriceTime
