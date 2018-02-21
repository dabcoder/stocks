{-# LANGUAGE OverloadedStrings #-}

module Net.Stocks
       ( Stock(..)
       , Company
       , getStocksResp
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

type Company = String

-- builds the URL
stocksQuery :: String -> String
stocksQuery company =
    "https://api.iextrading.com/1.0/stock/" ++ company ++ "/quote"

getStocksResp :: String -> IO (Maybe Stock)
getStocksResp company = do
    obj <- simpleHttp (stocksQuery company)
    return $ decode obj