{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Book (Book(..)) where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import GHC.Generics
import Net.IEX.Quote (Quote)

data Book = Book {
  quote :: Quote,
  bids :: Maybe [Trade], -- FIXME: not sure of this type
  asks :: Maybe [Trade], -- FIXME: not sure of this type
  trades :: [Trade]
} deriving (Generic, Show, Eq)

data Trade = Trade {
  price :: Double,
  size :: Integer,
  tradeId :: Integer,
  isISO :: Bool,
  isOddLot :: Bool,
  isOutsideRegularHours :: Bool,
  isSinglePriceCross :: Bool,
  isTradeThroughExempt :: Bool,
  timestamp :: Integer
} deriving (Generic, Show, Eq)

instance ToJSON Book
instance ToJSON Trade
instance FromJSON Book
instance FromJSON Trade
