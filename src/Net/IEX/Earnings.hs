{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Earnings (Earning(..),
                         Earnings(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data Earning = Earning {
  actualEPS :: Double,
  consensusEPS :: Double,
  estimatedEPS :: Double,
  announceTime :: String,
  numberOfEstimates :: Integer,
  epsSurpriseDollar :: Double,
  epsReportDate :: String,
  fiscalPeriod :: String,
  fiscalEndDate :: String
} deriving (Generic, Show, Eq)

data Earnings = Earnings {
  symbol :: String,
  earnings :: [Earning]
} deriving (Generic, Show, Eq)

customOptionsEarning =
  defaultOptions {
    fieldLabelModifier = let f "epsSurpriseDollar" = "EPSSurpriseDollar"
                             f "epsReportDate"     = "EPSReportDate"
                             f other = other
                         in f
    }

instance ToJSON Earnings
instance ToJSON Earning
instance FromJSON Earnings
instance FromJSON Earning where
  parseJSON = genericParseJSON customOptionsEarning
