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
type AuthAndSymbol = (String, Symbol)

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
baseURL = "https://cloud.iexapis.com/stable/stock/"

marketURL :: String
marketURL = "https://api.iextrading.com/1.0/market"

intraDayURL :: String
intraDayURL = "https://api.iextrading.com/1.0/stats/intraday"

statsURL :: String
statsURL = "https://api.iextrading.com/1.0/stats/"

lowerString :: Symbol -> String
lowerString = DL.map toLower

tokenize :: String -> String
tokenize auth = "?token=" ++ auth

getChart :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getChart5y :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart5y (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/5y" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getChart2y :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart2y (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/2y" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getChart1y :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart1y (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/1y" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getChart6m :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart6m (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/6m" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getChart3m :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart3m (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/3m" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getChart1m :: AuthAndSymbol -> IO (Maybe [IEXChart.Chart])
getChart1m (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/chart/1m" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right str ->
      return $ decode str

getCompany :: AuthAndSymbol -> IO (Maybe IEXCompany.Company)
getCompany (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/company" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getBook :: AuthAndSymbol -> IO (Maybe IEXBook.Book)
getBook (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/book" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getDelayedQuote :: AuthAndSymbol -> IO (Maybe IEXDelayedQuote.DelayedQuote)
getDelayedQuote (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/delayed-quote" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getDelayedDividend :: AuthAndSymbol -> IO (Maybe [IEXDividend.Dividend])
getDelayedDividend (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/dividends/5y" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getEarnings :: AuthAndSymbol -> IO (Maybe IEXEarnings.Earnings)
getEarnings (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/earnings" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getEffectiveSpread :: AuthAndSymbol -> IO (Maybe [IEXEffectiveSpread.EffectiveSpread])
getEffectiveSpread (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/effective-spread" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getFinancials :: AuthAndSymbol -> IO (Maybe IEXFinancials.Financials)
getFinancials (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/financials" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getStats :: AuthAndSymbol -> IO (Maybe IEXStats.Stats)
getStats (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/stats" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getNewsItem :: AuthAndSymbol -> IO (Maybe [IEXNewsItem.NewsItem])
getNewsItem (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/news/last/1" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getOHLC :: AuthAndSymbol -> IO (Maybe IEXOHLC.OHLC)
getOHLC (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/ohlc" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getPeers :: AuthAndSymbol -> IO (Maybe L8.ByteString)
getPeers (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/peers" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ Just bytestr

getPrevious :: AuthAndSymbol -> IO (Maybe IEXPrevious.Previous)
getPrevious (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/previous" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

-- FIXME: do not json parse an int
getPrice :: AuthAndSymbol -> IO (Maybe Double)
getPrice (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/price" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getQuote :: AuthAndSymbol -> IO (Maybe IEXQuote.Quote)
getQuote (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/quote" ++ tokenize auth)
  case obj of
    Left _ -> do
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getRelevant :: AuthAndSymbol -> IO (Maybe IEXRelevant.Relevant)
getRelevant (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/relevant" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getSplit :: AuthAndSymbol -> IO (Maybe [IEXSplit.Split])
getSplit (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/splits/5y" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getVolumeByVenue :: AuthAndSymbol -> IO (Maybe [IEXVolumeByVenue.VolumeByVenue])
getVolumeByVenue (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/volume-by-venue" ++ tokenize auth)
  case obj of
    Left _ ->
      return Nothing
    Right bytestr ->
      return $ decode bytestr

getTS :: AuthAndSymbol -> IO (Maybe [IEXTimeSeries.TimeSeries])
getTS (auth, symb) = do
  obj <- getNonJSONData (baseURL ++ lowerString symb ++ "/time-series" ++ tokenize auth)
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
getNonJSONData query = do
  putStrLn query
  try $ simpleHttp query
