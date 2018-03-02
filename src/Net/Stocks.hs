{-# LANGUAGE OverloadedStrings #-}

module Net.Stocks
       ( Stock(..)
       , Financials(..)
       , QueryType(..)
       , Company
       , getData
       ) where
  
import Control.Monad
import Control.Applicative
import Data.Aeson
import Network.HTTP.Conduit

-- | Stock data
data Stock =
    Stock { company          :: String  -- ^ The company's name
          , latestPrice      :: Float   -- ^ Latest stock price
          , latestTime       :: String  -- ^ Timeframe - string
          , changePercent    :: Float   -- ^ Percentage change
          } deriving (Show)

instance FromJSON Stock where
    parseJSON (Object v) = Stock
        <$> v .: "companyName"
        <*> v .: "latestPrice"
        <*> v .: "latestTime"
        <*> v .: "changePercent"
    parseJSON _          = mzero

-- | Financials data
data Financials = 
    Financials { reportDate       :: String  -- ^ The report's date
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

baseURL :: String
baseURL = "https://api.iextrading.com/1.0/stock/"

data QueryType = QueryStocks
               | QueryFinancials

-- builds the URL: /stock/{symbol}/quote
stocksQuery :: Company -> String
stocksQuery company = baseURL ++ company ++ "/quote"

-- builds the URL: /stock/{symbol}/financials
financialsQuery :: Company -> String
financialsQuery company = baseURL ++ company ++ "/financials"

-- get JSON data 
getData :: (FromJSON a) => String -> QueryType -> IO (Maybe a)
getData company qt = do
    obj <- simpleHttp (query qt company)
    return $ decode obj
  where
    query QueryStocks     = stocksQuery
    query QueryFinancials = financialsQuery
