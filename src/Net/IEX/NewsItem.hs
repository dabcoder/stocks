{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.NewsItem (NewsItem(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data NewsItem = NewsItem {
  datetime :: String,
  headline :: String,
  source :: String,
  url :: String,
  summary :: String,
  related :: String
} deriving (Generic, Show, Eq)

instance ToJSON NewsItem
instance FromJSON NewsItem
