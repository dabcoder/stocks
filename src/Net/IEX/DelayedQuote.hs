{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.DelayedQuote (DelayedQuote(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data DelayedQuote = DelayedQuote {
  symbol :: String,
  delayedPrice :: Double,
  high :: Double,
  low :: Double,
  delayedSize :: Double,
  delayedPriceTime :: Integer,
  processedTime :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON DelayedQuote
instance FromJSON DelayedQuote
