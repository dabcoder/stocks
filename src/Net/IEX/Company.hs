{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Company (Company(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data Company = Company {
  symbol :: String,
  companyName :: String,
  exchange :: String,
  industry :: String,
  website :: String,
  description :: String,
  ceo :: String,
  issueType :: String,
  sector :: String
} deriving (Generic, Show, Eq)

customOptionsCompany =
  defaultOptions {
    fieldLabelModifier = let f "ceo" = "CEO"
                             f other = other
                         in f
    }

instance ToJSON Company
instance FromJSON Company where
  parseJSON = genericParseJSON customOptionsCompany
