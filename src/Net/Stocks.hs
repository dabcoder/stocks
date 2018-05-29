{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Net.Stocks
       (
         getChart,
         getCompany,
         getBook,
         getDelayedQuote,
         getDelayedDividend,
         getEarnings,
         getEffectiveSpread,
         getFinancials,
         getStats,
         getNewsItem,
         getOHLC,
         getPrevious,
         getPeers,
         getPrice,
         getQuote,
         getRelevant,
         getSplit,
         getVolumeByVenue,
         getTS,
         getBatch,
         getBatchCompany,
         getMarket,
         getIntraDayStats,
         getRecentStats,
         getRecordStats,
         getHistoricalStats,
         -- getHistoricalDailyStats,
         typeQuery,
         Batch (..),
         BatchQuery (..)
       ) where

import System.IO
import GHC.Generics
import Data.Aeson
import Data.Char
import Data.HashMap.Strict
import Data.Maybe
import Network.HTTP.Conduit

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List                  as DL
import qualified Data.Map                   as DM

import qualified Net.IEX.Chart              as IEXChart
import qualified Net.IEX.Company            as IEXCompany
import qualified Net.IEX.Stats              as IEXStats
import qualified Net.IEX.Earnings           as IEXEarnings
import qualified Net.IEX.NewsItem           as IEXNewsItem
import qualified Net.IEX.DelayedQuote       as IEXDelayedQuote
import qualified Net.IEX.Dividend           as IEXDividend
import qualified Net.IEX.EffectiveSpread    as IEXEffectiveSpread
import qualified Net.IEX.Financials         as IEXFinancials
import qualified Net.IEX.OHLC               as IEXOHLC
import qualified Net.IEX.PriceTime          as IEXPriceTime
import qualified Net.IEX.Previous           as IEXPrevious
import qualified Net.IEX.Quote              as IEXQuote
import qualified Net.IEX.Split              as IEXSplit
import qualified Net.IEX.VolumeByVenue      as IEXVolumeByVenue
import qualified Net.IEX.Relevant           as IEXRelevant
import qualified Net.IEX.Market             as IEXMarket
import qualified Net.IEX.IntraDayStats      as IEXIntraDayStats
import qualified Net.IEX.RecentStats        as IEXRecentStats
import qualified Net.IEX.RecordStats        as IEXRecordStats
import qualified Net.IEX.Book               as IEXBook
import qualified Net.IEX.TimeSeries         as IEXTimeSeries

type Symbol = String

data BatchQuery =  NewsQuery            |
                   ChartQuery           |
                   CompanyQuery         |
                   DelayedQuoteQuery    |
                   DividendQuery        |
                   EarningsQuery        |
                   EffectiveSpreadQuery |
                   FinancialsQuery      |
                   StatsQuery           |
                   OHLCQuery            |
                   PriceTimeQuery       |
                   PreviousQuery        |
                   QuoteQuery           |
                   SplitQuery           |
                   VolumeByVenueQuery

batchQueryToStr :: BatchQuery -> String
batchQueryToStr NewsQuery = "news"
batchQueryToStr ChartQuery = "chart"
batchQueryToStr CompanyQuery = "company"
batchQueryToStr DelayedQuoteQuery = "delayedquote"
batchQueryToStr DividendQuery = "dividends"
batchQueryToStr EarningsQuery = "earnings"
batchQueryToStr EffectiveSpreadQuery = "effectivespread"
batchQueryToStr FinancialsQuery = "financials"
batchQueryToStr StatsQuery = "stats"
batchQueryToStr OHLCQuery = "ohlc"
batchQueryToStr PriceTimeQuery = "price"
batchQueryToStr QuoteQuery = "quote"
batchQueryToStr SplitQuery = "split"
batchQueryToStr VolumeByVenueQuery = "volumebyvenue"

data Batch = Batch {
  news :: Maybe [IEXNewsItem.NewsItem],
  chart :: Maybe [IEXChart.Chart],
  company :: Maybe IEXCompany.Company,
  delayedQuote :: Maybe IEXDelayedQuote.DelayedQuote,
  dividend :: Maybe [IEXDividend.Dividend],
  earnings :: Maybe IEXEarnings.Earnings,
  effectiveSpread :: Maybe [IEXEffectiveSpread.EffectiveSpread],
  financials :: Maybe IEXFinancials.Financials,
  stats :: Maybe IEXStats.Stats,
  ohlc :: Maybe IEXOHLC.OHLC,
  priceTime :: Maybe Integer,
  previous :: Maybe IEXPrevious.Previous,
  quote :: Maybe IEXQuote.Quote,
  split :: Maybe [IEXSplit.Split],
  volumeByVenue :: Maybe [IEXVolumeByVenue.VolumeByVenue]
} deriving (Generic, Show, Eq)

-- ToJSON means taking a haskell data structure and making a JSON string
instance ToJSON Batch

-- FromJSON means parsing the text into a haskell data structure
instance FromJSON Batch

baseURL :: String
baseURL = "https://api.iextrading.com/1.0/stock/"

marketURL :: String
marketURL = "https://api.iextrading.com/1.0/market"

intraDayURL :: String
intraDayURL = "https://api.iextrading.com/1.0/stats/intraday"

statsURL :: String
statsURL = "https://api.iextrading.com/1.0/stats/"

lowerString :: Symbol -> String
lowerString = DL.map toLower

getChart :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart")
  return $ decode obj

getCompany :: Symbol -> IO (Maybe IEXCompany.Company)
getCompany symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/company")
  return $ decode obj

getBook :: Symbol -> IO (Maybe IEXBook.Book)
getBook symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/book")
  return $ decode obj

getDelayedQuote :: Symbol -> IO (Maybe IEXDelayedQuote.DelayedQuote)
getDelayedQuote symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/delayed-quote")
  return $ decode obj

getDelayedDividend :: Symbol -> IO (Maybe [IEXDividend.Dividend])
getDelayedDividend symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/dividends/5y")
  return $ decode obj

getEarnings :: Symbol -> IO (Maybe IEXEarnings.Earnings)
getEarnings symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/earnings")
  return $ decode obj

getEffectiveSpread :: Symbol -> IO (Maybe [IEXEffectiveSpread.EffectiveSpread])
getEffectiveSpread symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/effective-spread")
  return $ decode obj

getFinancials :: Symbol -> IO (Maybe IEXFinancials.Financials)
getFinancials symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/financials")
  return $ decode obj

getStats :: Symbol -> IO (Maybe IEXStats.Stats)
getStats symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/stats")
  return $ decode obj

getNewsItem :: Symbol -> IO (Maybe [IEXNewsItem.NewsItem])
getNewsItem symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/news/last/1")
  return $ decode obj

getOHLC :: Symbol -> IO (Maybe IEXOHLC.OHLC)
getOHLC symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/ohlc")
  return $ decode obj

getPeers :: Symbol -> IO L8.ByteString
getPeers symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/peers")
  return $ obj

getPrevious :: Symbol -> IO (Maybe IEXPrevious.Previous)
getPrevious symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/previous")
  return $ decode obj

-- FIXME: do not json parse an int
getPrice :: Symbol -> IO (Maybe Double)
getPrice symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/price")
  return $ decode obj

getQuote :: Symbol -> IO (Maybe IEXQuote.Quote)
getQuote symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/quote")
  return $ decode obj

getRelevant :: Symbol -> IO (Maybe IEXRelevant.Relevant)
getRelevant symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/relevant")
  return $ decode obj

getSplit :: Symbol -> IO (Maybe [IEXSplit.Split])
getSplit symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/splits/5y")
  return $ decode obj

getVolumeByVenue :: Symbol -> IO (Maybe [IEXVolumeByVenue.VolumeByVenue])
getVolumeByVenue symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/volume-by-venue")
  return $ decode obj

getTS :: Symbol -> IO (Maybe [IEXTimeSeries.TimeSeries])
getTS symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/time-series")
  return $ decode obj

-- get a list of parts we want in a batch request, and translate that
-- to HTTP parameters to provide to the API call
typeQuery :: [BatchQuery] -> String
typeQuery [] = ""
typeQuery inp =
  "types=" ++ (concat $ DL.intersperse "," (fmap batchQueryToStr inp))

symbolQuery :: [Symbol] -> String
symbolQuery [] = ""
symbolQuery inp = "symbols=" ++ (concat $ DL.intersperse "," inp)

questionMark :: [BatchQuery] -> String
questionMark [] = ""
questionMark _  = "?"

getBatch :: [Symbol] -> [BatchQuery] -> IO (Maybe (DM.Map String Batch))
getBatch symbs queryParams =
  let urlPt = baseURL ++ "market/batch?"
      fullQuery = urlPt ++ symbolQuery symbs ++ "&" ++ typeQuery queryParams
  in do
    obj <- getNonJSONData fullQuery
    return $ decode obj

-- batch query of a *single* company
getBatchCompany :: Symbol -> [BatchQuery] -> IO (Maybe Batch)
getBatchCompany symb queryParams =
  let urlPt = (baseURL ++ lowerString symb ++ "/batch/")
      fullQuery = urlPt ++ (questionMark queryParams) ++ (typeQuery queryParams)
  in do
    obj <- getNonJSONData fullQuery
    return $ decode obj

getMarket :: IO (Maybe [IEXMarket.Market])
getMarket = do
    obj <- getNonJSONData marketURL
    return $ decode obj

getIntraDayStats :: IO (Maybe IEXIntraDayStats.IntraDayStats)
getIntraDayStats = do
    obj <- getNonJSONData intraDayURL
    return $ decode obj

getRecentStats :: IO (Maybe [IEXRecentStats.RecentStats])
getRecentStats = do
    obj <- getNonJSONData (statsURL ++ "recent")
    return $ decode obj

getRecordStats :: IO (Maybe IEXRecordStats.RecordStats)
getRecordStats = do
    obj <- getNonJSONData (statsURL ++ "records")
    return $ decode obj

getHistoricalStats :: IO (Maybe [IEXRecentStats.RecentStats])
getHistoricalStats = undefined

-- currently does not work due to inconsitency in
-- IEX API. isHalfDay is Bool in one API call and Int in another

-- getHistoricalDailyStats :: IO (Maybe [IEXRecentStats.RecentStats])
-- getHistoricalDailyStats = do
--     obj <- getNonJSONData (statsURL ++ "historical/daily")
--     return $ decode obj

getNonJSONData :: String -> IO L8.ByteString
getNonJSONData query = do
    obj <- simpleHttp query
    return obj
