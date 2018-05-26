{-# LANGUAGE DeriveGeneric #-}

module Net.IEX.Relevant (Relevant(..)) where

import Data.Maybe
import Data.Aeson
import GHC.Generics

data Relevant = Relevant {
  peers :: Bool,
  symbols :: [String]
} deriving (Generic, Show, Eq)

instance ToJSON Relevant
instance FromJSON Relevant
