{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Stats (Stats (..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data Stats = Stats {
  companyName :: String,
  marketcap :: Integer,
  beta :: Double,
  week52high :: Double,
  week52low :: Double,
  week52change :: Double,
  shortInterest :: Integer,
  shortDate :: String,
  dividendRate :: Double,
  dividendYield :: Double,
  exDividendDate :: String,
  latestEPS :: Double,
  latestEPSDate :: String,
  sharesOutstanding :: Integer,
  float :: Integer,
  returnOnEquity :: Double,
  consensusEPS :: Double,
  numberOfEstimates :: Integer,
  epsSurpriseDollar :: Maybe Double,
  epsSurprisePercent :: Maybe Double,
  symbol :: String,
  ebitda :: Integer,
  revenue :: Integer,
  grossProfit :: Integer,
  cash :: Integer,
  debt :: Integer,
  ttmEPS :: Double,
  revenuePerShare :: Integer,
  revenuePerEmployee :: Integer,
  peRatioHigh :: Double,
  peRatioLow :: Double,
  returnOnAssets :: Double,
  returnOnCapital :: Maybe Double,
  profitMargin :: Double,
  priceToSales :: Double,
  priceToBook :: Double,
  day200MovingAvg :: Double,
  day50MovingAvg :: Double,
  institutionPercent :: Double,
  insiderPercent :: Maybe Double,
  shortRatio :: Maybe Double,
  year5ChangePercent :: Double,
  year2ChangePercent :: Double,
  year1ChangePercent :: Double,
  ytdChangePercent :: Double,
  month6ChangePercent :: Double,
  month3ChangePercent :: Double,
  month1ChangePercent :: Double,
  day5ChangePercent :: Double,
  day30ChangePercent :: Double
} deriving (Generic, Show, Eq)

customOptionsStats =
  defaultOptions {
    fieldLabelModifier = let f "epsSurpriseDollar"  = "EPSSurpriseDollar"
                             f "epsSurprisePercent" = "EPSSurprisePercent"
                             f "ebitda"             = "EBITDA"
                             f other = other
                         in f
    }

instance ToJSON Stats
instance FromJSON Stats where
  parseJSON = genericParseJSON customOptionsStats
