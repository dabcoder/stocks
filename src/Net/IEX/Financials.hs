{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Financials (Financials(..),
                           Financial(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data Financial = Financial {
  reportDate :: String,
  grossProfit :: Integer,
  costOfRevenue :: Integer,
  operatingRevenue :: Integer,
  totalRevenue :: Integer,
  operatingIncome :: Integer,
  netIncome :: Integer,
  researchAndDevelopment :: Integer,
  operatingExpense :: Integer,
  currentAssets :: Integer,
  totalAssets :: Integer,
  totalLiabilities :: Maybe Integer,
  currentCash :: Integer,
  currentDebt :: Integer,
  totalCash :: Integer,
  totalDebt :: Integer,
  shareholderEquity :: Integer,
  cashChange :: Integer,
  cashFlow :: Integer,
  operatingGainsLosses :: Maybe String
} deriving (Generic, Show, Eq)

data Financials = Financials {
  symbol :: String,
  financials :: [Financial]
} deriving (Generic, Show, Eq)

instance ToJSON Financial
instance ToJSON Financials
instance FromJSON Financial
instance FromJSON Financials
