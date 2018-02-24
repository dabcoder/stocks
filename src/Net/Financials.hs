{-# LANGUAGE OverloadedStrings #-}

module Net.Financials
       ( Financials(..)
       , Company
       , getFinancialsResp
       ) where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Network.HTTP.Conduit

-- | Financials data
data Financials = 
    Financials { reportDate       :: String  -- ^ The report's name
               , grossProfit      :: Int     -- ^ Gross Profit
               , costOfRevenue    :: Int     -- ^ Cost of revenue
               , cashFlow         :: Int     -- ^ Cash flow
               } deriving (Show)

-- https://stackoverflow.com/questions/16547783/parse-array-in-nested-json-with-aeson
instance FromJSON Financials where
    parseJSON (Object v) = do
        financialsV <- head <$> v.: "financials"
        Financials <$> financialsV .: "reportDate"
                   <*> financialsV .: "grossProfit"
                   <*> financialsV .: "costOfRevenue"
                   <*> financialsV .: "cashFlow"
    parseJSON _          = mzero

type Company = String

-- builds the URL: /stock/{symbol}/financials
financialsQuery :: Company -> String
financialsQuery company =
    "https://api.iextrading.com/1.0/stock/" ++ company ++ "/financials"

-- get JSON data 
getFinancialsResp :: String -> IO (Maybe Financials)
getFinancialsResp company = do
    obj <- simpleHttp (financialsQuery company)
    return $ decode obj

