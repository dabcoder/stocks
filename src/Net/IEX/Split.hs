{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Split (Split(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics

data Split = Split {
  exDate :: String,
  declaredDate :: String,
  recordDate :: String,
  paymentDate :: String,
  ratio :: Double,
  toFactor :: Integer,
  forFactor :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON Split
instance FromJSON Split
