{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Quote (Quote(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data Quote = Quote {
  symbol :: String,
  companyName :: String,
  primaryExchange :: Maybe String,
  sector :: Maybe String,
  calculationPrice :: String,
  open :: Double,
  openTime :: Integer,
  close :: Double,
  closeTime :: Integer,
  high :: Double,
  low :: Double,
  latestPrice :: Double,
  latestSource :: String,
  latestTime :: String,
  latestUpdate :: Integer,
  latestVolume :: Integer,
  iexRealtimePrice :: Maybe Double,
  iexRealtimeSize :: Maybe Integer,
  iexLastUpdated :: Maybe Integer,
  delayedPrice :: Double,
  delayedPriceTime :: Integer,
  extendedPrice :: Double,
  extendedChange :: Double,
  extendedChangePercent :: Double,
  extendedPriceTime :: Integer,
  previousClose :: Double,
  change :: Double,
  changePercent :: Double,
  iexMarketPercent :: Maybe Double,
  iexVolume :: Maybe Integer,
  avgTotalVolume :: Integer,
  iexBidPrice :: Maybe Double,
  iexBidSize :: Maybe Integer,
  iexAskPrice :: Maybe Double,
  iexAskSize :: Maybe Integer,
  marketCap :: Integer,
  peRatio :: Double,
  week52High :: Double,
  week52Low :: Double,
  ytdChange :: Double
} deriving (Generic, Show, Eq)

instance ToJSON Quote
instance FromJSON Quote
