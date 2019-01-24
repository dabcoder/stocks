{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Net.Stocks
       (
         getChart,
         getChart5y,
         getChart2y,
         getChart1y,
         getChart6m,
         getChart3m,
         getChart1m,
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
import Control.Exception
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
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str


getChart5y :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart5y symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/5y")
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str


getChart2y :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart2y symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/2y")
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str 


getChart1y :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart1y symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/1y")
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str


getChart6m :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart6m symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/6m")
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str


getChart3m :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart3m symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/3m")
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str


getChart1m :: Symbol -> IO (Maybe [IEXChart.Chart])
getChart1m symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/1m")
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

  
getCompany :: Symbol -> IO (Maybe IEXCompany.Company)
getCompany symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/company")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getBook :: Symbol -> IO (Maybe IEXBook.Book)
getBook symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/book")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getDelayedQuote :: Symbol -> IO (Maybe IEXDelayedQuote.DelayedQuote)
getDelayedQuote symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/delayed-quote")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getDelayedDividend :: Symbol -> IO (Maybe [IEXDividend.Dividend])
getDelayedDividend symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/dividends/5y")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getEarnings :: Symbol -> IO (Maybe IEXEarnings.Earnings)
getEarnings symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/earnings")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getEffectiveSpread :: Symbol -> IO (Maybe [IEXEffectiveSpread.EffectiveSpread])
getEffectiveSpread symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/effective-spread")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getFinancials :: Symbol -> IO (Maybe IEXFinancials.Financials)
getFinancials symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/financials")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getStats :: Symbol -> IO (Maybe IEXStats.Stats)
getStats symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/stats")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getNewsItem :: Symbol -> IO (Maybe [IEXNewsItem.NewsItem])
getNewsItem symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/news/last/1")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getOHLC :: Symbol -> IO (Maybe IEXOHLC.OHLC)
getOHLC symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/ohlc")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getPeers :: Symbol -> IO (Maybe L8.ByteString)
getPeers symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/peers")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ Just bytestr

getPrevious :: Symbol -> IO (Maybe IEXPrevious.Previous)
getPrevious symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/previous")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

-- FIXME: do not json parse an int
getPrice :: Symbol -> IO (Maybe Double)
getPrice symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/price")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getQuote :: Symbol -> IO (Maybe IEXQuote.Quote)
getQuote symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/quote")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getRelevant :: Symbol -> IO (Maybe IEXRelevant.Relevant)
getRelevant symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/relevant")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getSplit :: Symbol -> IO (Maybe [IEXSplit.Split])
getSplit symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/splits/5y")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getVolumeByVenue :: Symbol -> IO (Maybe [IEXVolumeByVenue.VolumeByVenue])
getVolumeByVenue symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/volume-by-venue")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getTS :: Symbol -> IO (Maybe [IEXTimeSeries.TimeSeries])
getTS symb = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/time-series")
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

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
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

-- batch query of a *single* company
getBatchCompany :: Symbol -> [BatchQuery] -> IO (Maybe Batch)
getBatchCompany symb queryParams =
  let urlPt = (baseURL ++ lowerString symb ++ "/batch/")
      fullQuery = urlPt ++ (questionMark queryParams) ++ (typeQuery queryParams)
  in do
    obj <- getNonJSONData fullQuery
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getMarket :: IO (Maybe [IEXMarket.Market])
getMarket = do
  obj <- getNonJSONData marketURL
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getIntraDayStats :: IO (Maybe IEXIntraDayStats.IntraDayStats)
getIntraDayStats = do
    obj <- getNonJSONData intraDayURL
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getRecentStats :: IO (Maybe [IEXRecentStats.RecentStats])
getRecentStats = do
    obj <- getNonJSONData (statsURL ++ "recent")
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getRecordStats :: IO (Maybe IEXRecordStats.RecordStats)
getRecordStats = do
    obj <- getNonJSONData (statsURL ++ "records")
    case obj of
      Left _ ->
        return Nothing
      Right bytestr ->
        return $ decode bytestr

getHistoricalStats :: IO (Maybe [IEXRecentStats.RecentStats])
getHistoricalStats = undefined

-- currently does not work due to inconsitency in
-- IEX API. isHalfDay is Bool in one API call and Int in another

-- getHistoricalDailyStats :: IO (Maybe [IEXRecentStats.RecentStats])
-- getHistoricalDailyStats = do
--     obj <- getNonJSONData (statsURL ++ "historical/daily")
--     return $ decode obj

getNonJSONData :: String -> IO (Either SomeException L8.ByteString)
getNonJSONData query = try $ simpleHttp query
