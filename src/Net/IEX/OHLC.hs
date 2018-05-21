{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.OHLC (OHLC(..)) where

import Net.IEX.PriceTime

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data OHLC = OHLC {
  open :: PriceTime,
  close :: PriceTime,
  high :: Double,
  low :: Double
} deriving (Generic, Show, Eq)

instance ToJSON OHLC
instance FromJSON OHLC
