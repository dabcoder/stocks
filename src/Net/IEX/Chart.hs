{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Chart (Chart(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data Chart = Chart {
  -- is only available on 1d chart.
  minute :: Maybe String,
  -- is only available on 1d chart. 15 minute delayed
  marketAverage :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketNotional :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketNumberOfTrades :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketHigh :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketLow :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketVolume :: Maybe Double,
  -- is only available on 1d chart. Percent change
  -- of each interval relative to first value. 15 minute delayed
  marketChangeOverTime :: Maybe Double,
  -- is only available on 1d chart.
  average :: Maybe Double,
  -- is only available on 1d chart.
  notional :: Maybe Double,
  -- is only available on 1d chart.
  numberOfTrades :: Maybe Double,
  -- is only available on 1d chart, and only when chartSimplify is true.
  -- The first element is the original number of points.
  -- Second element is how many remain after simplification.
  simplifyFactor :: Maybe [Integer],
  -- is available on all charts.
  high :: Double,
  -- is available on all charts.
  low :: Double,
  -- is available on all charts.
  volume :: Integer,
  -- is available on all charts. A variable formatted version of
  -- the date depending on the range. Optional convienience field.
  label :: String,
  -- is available on all charts. Percent change of each interval
  -- relative to first value. Useful for comparing multiple stocks.
  changeOverTime :: Double,
  -- is not available on 1d chart.
  date :: Maybe String,
  -- is not available on 1d chart.
  open :: Maybe Double,
  -- is not available on 1d chart.
  close :: Maybe Double,
  -- is not available on 1d chart.
  unadjustedVolume :: Maybe Integer,
  -- is not available on 1d chart.
  change :: Maybe Double,
  -- is not available on 1d chart.
  changePercent :: Maybe Double,
  -- is not available on 1d chart.
  vwap :: Maybe Double
} deriving (Generic, Show, Eq)

instance ToJSON Chart
instance FromJSON Chart
