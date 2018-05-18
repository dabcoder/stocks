{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- missing:

--  * batch (https://iextrading.com/developer/docs/#batch-requests)
--  * book (https://iextrading.com/developer/docs/#book)
--  * chart (to be able to give different options to IEX,
--           5y, 2y, 1y
--           https://iextrading.com/developer/docs/#chart)
--  * IEX Regulation SHO Threshold Securities List
--        (https://iextrading.com/developer/docs/#iex-regulation-sho-\
--         threshold-securities-list)
--  * IEX Short Interest List
--        (https://iextrading.com/developer/docs/#iex-short-interest-list)
--  * logo (https://iextrading.com/developer/docs/#logo)
--  * time series (https://iextrading.com/developer/docs/#time-series)

--  * Reference data (https://iextrading.com/developer/docs/#reference-data)
--    * Symbols
--    * IEX Corporate Actions
--    * IEX Dividends
--    * IEX Next Day Ex Date
--    * IEX Listed Symbol Directory

-- * IEX market data (https://iextrading.com/developer/docs/#iex-market-data)
--    * TOPS
--    * Last
--    * HIST
--    * DEEP
--    * Book
--    * Trades
--    * System Event
--    * Trading Status
--    * Operational Halt Status
--    * Short Sale Price Test Status
--    * Security Event
--    * Trade Break
--    * Auction
--    * Official Price
--        (https://iextrading.com/developer/docs/#iex-market-data)

-- * IEX Stats (https://iextrading.com/developer/docs/#iex-stats)
--    * Intraday
--    * Recent
--    * Records
--    * Historical Summary
--    * Historical Daily

-- * Markets
--    * Market (https://iextrading.com/developer/docs/#market)

-- general FIXME: which fields in which records need to be Maybe?
-- general FIXME: reduce copy paste coding?
-- general FIXME: needs better, more specific testing

module Net.Stocks
       (
         getChart,
         getCompany,
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
         batchQueryToStr,
         Chart,
         Company,
         DelayedQuote,
         Dividend,
         Earning,
         Earnings,
         EffectiveSpread,
         Financial,
         Financials,
         Stats,
         NewsItem,
         OHLC,
         PriceTime,
         Previous,
         Quote,
         Relevant,
         Split,
         VolumeByVenue,
         Ticker,
         BatchQuery (..)
       ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           System.IO
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.List
import           Data.Maybe
import           Network.HTTP.Conduit

type Ticker = String

data BatchQuery =
  NewsQuery |
  ChartQuery |
  CompanyQuery |
  DelayedQuoteQuery |
  DividendQuery |
  EarningsQuery |
  EffectiveSpreadQuery |
  FinancialsQuery |
  StatsQuery |
  OHLCQuery |
  PriceTimeQuery |
  PreviousQuery |
  QuoteQuery |
  SplitQuery |
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

-- get a list of parts we want in a batch request, and translate that
-- to HTTP parameters to provide to the API call
typeQuery :: [BatchQuery] -> String
typeQuery [] = ""
typeQuery inp = '&' : (concat $ intersperse "," (fmap batchQueryToStr inp))

data Batch = Batch {
  news :: Maybe [NewsItem],
  chart :: Maybe [Chart],
  company :: Maybe Company,
  delayedQuote :: Maybe DelayedQuote,
  dividend :: Maybe [Dividend],
  earnings :: Maybe Earnings,
  effectiveSpread :: Maybe [EffectiveSpread],
  financials :: Maybe Financials,
  stats :: Maybe Stats,
  ohlc :: Maybe OHLC,
  priceTime :: Maybe Integer,
  previous :: Maybe Previous,
  quote :: Maybe Quote,
  split :: Maybe [Split],
  volumeByVenue :: Maybe [VolumeByVenue]
} deriving (Generic, Show, Eq)

getBatch :: [BatchQuery] -> IO Batch
getBatch = undefined

data Chart = Chart {
  -- is only available on 1d chart.
  minute :: Maybe String,
  -- is only available on 1d chart. 15 minute delayed
  marketAverage :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketNotional :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketNumberOfTrades :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketHigh :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketLow :: Maybe Double,
  -- is only available on 1d chart. 15 minute delayed
  marketVolume :: Maybe Double,
  -- is only available on 1d chart. Percent change
  -- of each interval relative to first value. 15 minute delayed
  marketChangeOverTime :: Maybe Double,
  -- is only available on 1d chart.
  average :: Maybe Double,
  -- is only available on 1d chart.
  notional :: Maybe Double,
  -- is only available on 1d chart.
  numberOfTrades :: Maybe Double,
  -- is only available on 1d chart, and only when chartSimplify is true.
  -- The first element is the original number of points.
  -- Second element is how many remain after simplification.
  simplifyFactor :: Maybe [Integer],
  -- is available on all charts.
  high :: Double,
  -- is available on all charts.
  low :: Double,
  -- is available on all charts.
  volume :: Integer,
  -- is available on all charts. A variable formatted version of
  -- the date depending on the range. Optional convienience field.
  label :: String,
  -- is available on all charts. Percent change of each interval
  -- relative to first value. Useful for comparing multiple stocks.
  changeOverTime :: Double,
  -- is not available on 1d chart.
  date :: Maybe String,
  -- is not available on 1d chart.
  open :: Maybe Double,
  -- is not available on 1d chart.
  close :: Maybe Double,
  -- is not available on 1d chart.
  unadjustedVolume :: Maybe Integer,
  -- is not available on 1d chart.
  change :: Maybe Double,
  -- is not available on 1d chart.
  changePercent :: Maybe Double,
  -- is not available on 1d chart.
  vwap :: Maybe Double
} deriving (Generic, Show, Eq)

data Company = Company {
  symbol :: String,
  companyName :: String,
  exchange :: String,
  industry :: String,
  website :: String,
  description :: String,
  ceo :: String,
  issueType :: String,
  sector :: String
} deriving (Generic, Show, Eq)

data DelayedQuote = DelayedQuote {
  symbol :: String,
  delayedPrice :: Double,
  high :: Double,
  low :: Double,
  delayedSize :: Double,
  delayedPriceTime :: Integer,
  processedTime :: Integer
} deriving (Generic, Show, Eq)

data Dividend = Dividend {
  exDate :: String,
  paymentDate :: String,
  recordDate :: String,
  declaredDate :: String,
  amount :: Double,
  flag :: String,
  dtype :: String,
  qualified :: String,
  indicated :: String
} deriving (Generic, Show, Eq)

data Earning = Earning {
  actualEPS :: Double,
  consensusEPS :: Double,
  estimatedEPS :: Double,
  announceTime :: String,
  numberOfEstimates :: Int,
  epsSurpriseDollar :: Double,
  epsReportDate :: String,
  fiscalPeriod :: String,
  fiscalEndDate :: String
} deriving (Generic, Show, Eq)

data Earnings = Earnings {
  symbol :: String,
  earnings :: [Earning]
} deriving (Generic, Show, Eq)

data EffectiveSpread = EffectiveSpread {
  volume :: Integer,
  venue :: String,
  venueName :: String,
  effectiveSpread :: Double,
  effectiveQuoted :: Double,
  priceImprovement :: Double
} deriving (Generic, Show, Eq)

data Financial = Financial {
  reportDate :: String,
  grossProfit :: Integer,
  costOfRevenue :: Integer,
  operatingRevenue :: Integer,
  totalRevenue :: Integer,
  operatingIncome :: Integer,
  netIncome :: Integer,
  researchAndDevelopment :: Integer,
  operatingExpense :: Integer,
  currentAssets :: Integer,
  totalAssets :: Integer,
  totalLiabilities :: Maybe Integer,
  currentCash :: Integer,
  currentDebt :: Integer,
  totalCash :: Integer,
  totalDebt :: Integer,
  shareholderEquity :: Integer,
  cashChange :: Integer,
  cashFlow :: Integer,
  operatingGainsLosses :: Maybe String
} deriving (Generic, Show, Eq)

data Financials = Financials {
  symbol :: String,
  financials :: [Financial]
} deriving (Generic, Show, Eq)

data Stats = Stats {
  companyName :: String,
  marketcap :: Integer,
  beta :: Double,
  week52high :: Double,
  week52low :: Double,
  week52change :: Double,
  shortInterest :: Integer,
  shortDate :: String,
  dividendRate :: Double,
  dividendYield :: Double,
  exDividendDate :: String,
  latestEPS :: Double,
  latestEPSDate :: String,
  sharesOutstanding :: Integer,
  float :: Integer,
  returnOnEquity :: Double,
  consensusEPS :: Double,
  numberOfEstimates :: Integer,
  epsSurpriseDollar :: Maybe Double,
  epsSurprisePercent :: Maybe Double,
  symbol :: String,
  ebitda :: Integer,
  revenue :: Integer,
  grossProfit :: Integer,
  cash :: Integer,
  debt :: Integer,
  ttmEPS :: Double,
  revenuePerShare :: Integer,
  revenuePerEmployee :: Integer,
  peRatioHigh :: Double,
  peRatioLow :: Double,
  returnOnAssets :: Double,
  returnOnCapital :: Maybe Double,
  profitMargin :: Double,
  priceToSales :: Double,
  priceToBook :: Double,
  day200MovingAvg :: Double,
  day50MovingAvg :: Double,
  institutionPercent :: Double,
  insiderPercent :: Maybe Double,
  shortRatio :: Maybe Double,
  year5ChangePercent :: Double,
  year2ChangePercent :: Double,
  year1ChangePercent :: Double,
  ytdChangePercent :: Double,
  month6ChangePercent :: Double,
  month3ChangePercent :: Double,
  month1ChangePercent :: Double,
  day5ChangePercent :: Double,
  day30ChangePercent :: Double
} deriving (Generic, Show, Eq)

data NewsItem = NewsItem {
  datetime :: String,
  headline :: String,
  source :: String,
  url :: String,
  summary :: String,
  related :: String
} deriving (Generic, Show, Eq)

data OHLC = OHLC {
  open :: PriceTime,
  close :: PriceTime,
  high :: Double,
  low :: Double
} deriving (Generic, Show, Eq)

data PriceTime = PriceTime {
  price :: Double,
  time :: Integer
} deriving (Generic, Show, Eq)

data Previous = Previous {
  symbol :: String,
  date :: String,
  open :: Double,
  high :: Double,
  low :: Double,
  close :: Double,
  volume :: Integer,
  unadjustedVolume :: Integer,
  change :: Double,
  changePercent :: Double,
  vwap :: Double
} deriving (Generic, Show, Eq)

data Quote = Quote {
  symbol :: String,
  companyName :: String,
  primaryExchange :: String,
  sector :: String,
  calculationPrice :: String,
  open :: Double,
  openTime :: Integer,
  close :: Double,
  closeTime :: Integer,
  high :: Double,
  low :: Double,
  latestPrice :: Double,
  latestSource :: String,
  latestTime :: String,
  latestUpdate :: Integer,
  latestVolume :: Integer,
  iexRealtimePrice :: Maybe Double,
  iexRealtimeSize :: Maybe Integer,
  iexLastUpdated :: Maybe Integer,
  delayedPrice :: Double,
  delayedPriceTime :: Integer,
  previousClose :: Double,
  change :: Double,
  changePercent :: Double,
  iexMarketPercent :: Maybe Double,
  iexVolume :: Maybe Integer,
  avgTotalVolume :: Integer,
  iexBidPrice :: Maybe Double,
  iexBidSize :: Maybe Integer,
  iexAskPrice :: Maybe Double,
  iexAskSize :: Maybe Integer,
  marketCap :: Integer,
  peRatio :: Double,
  week52High :: Double,
  week52Low :: Double,
  ytdChange :: Double
} deriving (Generic, Show, Eq)

data Relevant = Relevant {
  peers :: Bool,
  symbols :: [String]
} deriving (Generic, Show, Eq)

data Split = Split {
  exDate :: String,
  declaredDate :: String,
  recordDate :: String,
  paymentDate :: String,
  ratio :: Double,
  toFactor :: Integer,
  forFactor :: Integer
} deriving (Generic, Show, Eq)

data VolumeByVenue = VolumeByVenue {
  volume :: Integer,
  venue :: String,
  venueName :: String,
  date :: Maybe String,
  marketPercent :: Double,
  avgMarketPercent :: Double
} deriving (Generic, Show, Eq)

-- special handling of label names
customOptionsCompany =
  defaultOptions {
    fieldLabelModifier = let f "ceo" = "CEO"
                             f other = other
                         in f
    }

customOptionsDividend =
  defaultOptions {
    fieldLabelModifier = let f "dtype" = "type"
                             f other = other
                         in f
    }

customOptionsEarning =
  defaultOptions {
    fieldLabelModifier = let f "epsSurpriseDollar" = "EPSSurpriseDollar"
                             f "epsReportDate"     = "EPSReportDate"
                             f other = other
                         in f
    }

customOptionsStats =
  defaultOptions {
    fieldLabelModifier = let f "epsSurpriseDollar"  = "EPSSurpriseDollar"
                             f "epsSurprisePercent" = "EPSSurprisePercent"
                             f "ebitda"             = "EBITDA"
                             f other = other
                         in f
    }

-- ToJSON means taking a haskell data structure and making a JSON string
instance ToJSON Chart
instance ToJSON Company
instance ToJSON DelayedQuote
instance ToJSON Dividend
instance ToJSON Earnings
instance ToJSON Earning
instance ToJSON EffectiveSpread
instance ToJSON Financial
instance ToJSON Financials
instance ToJSON Stats
instance ToJSON NewsItem
instance ToJSON OHLC
instance ToJSON PriceTime
instance ToJSON Previous
instance ToJSON Quote
instance ToJSON Relevant
instance ToJSON Split
instance ToJSON VolumeByVenue

-- FromJSON means parsing the text into a haskell data structure
instance FromJSON Chart
instance FromJSON Company where
  parseJSON = genericParseJSON customOptionsCompany
instance FromJSON DelayedQuote
instance FromJSON Dividend where
  parseJSON = genericParseJSON customOptionsDividend
instance FromJSON Earnings
instance FromJSON Earning where
  parseJSON = genericParseJSON customOptionsEarning
instance FromJSON EffectiveSpread
instance FromJSON Financial
instance FromJSON Financials
instance FromJSON Stats where
  parseJSON = genericParseJSON customOptionsStats
instance FromJSON NewsItem
instance FromJSON OHLC
instance FromJSON PriceTime
instance FromJSON Previous
instance FromJSON Quote
instance FromJSON Relevant
instance FromJSON Split
instance FromJSON VolumeByVenue

baseURL :: String
baseURL = "https://api.iextrading.com/1.0/stock/"

lowerString :: Ticker -> String
lowerString = map toLower

getChart :: Ticker -> IO (Maybe [Chart])
getChart ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/chart")
  return $ decode obj

getCompany :: Ticker -> IO (Maybe Company)
getCompany ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/company")
  return $ decode obj

getDelayedQuote :: Ticker -> IO (Maybe DelayedQuote)
getDelayedQuote ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/delayed-quote")
  return $ decode obj

getDelayedDividend :: Ticker -> IO (Maybe [Dividend])
getDelayedDividend ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/dividends/5y")
  return $ decode obj

getEarnings :: Ticker -> IO (Maybe Earnings)
getEarnings ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/earnings")
  return $ decode obj

getEffectiveSpread :: Ticker -> IO (Maybe [EffectiveSpread])
getEffectiveSpread ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/effective-spread")
  return $ decode obj

getFinancials :: Ticker -> IO (Maybe Financials)
getFinancials ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/financials")
  return $ decode obj

getStats :: Ticker -> IO (Maybe Stats)
getStats ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/stats")
  return $ decode obj

getNewsItem :: Ticker -> IO (Maybe [NewsItem])
getNewsItem ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/news/last/1")
  return $ decode obj

getOHLC :: Ticker -> IO (Maybe OHLC)
getOHLC ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/ohlc")
  return $ decode obj

getPeers :: Ticker -> IO L8.ByteString
getPeers ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/peers")
  return $ obj

getPrevious :: Ticker -> IO (Maybe Previous)
getPrevious ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/previous")
  return $ decode obj

-- FIXME: do not json parse an int
getPrice :: Ticker -> IO (Maybe Integer)
getPrice ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/price")
  return $ decode obj

getQuote :: Ticker -> IO (Maybe Quote)
getQuote ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/quote")
  return $ decode obj

getRelevant :: Ticker -> IO (Maybe Relevant)
getRelevant ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/relevant")
  return $ decode obj

getSplit :: Ticker -> IO (Maybe [Split])
getSplit ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/splits/5y")
  return $ decode obj

getVolumeByVenue :: Ticker -> IO (Maybe [VolumeByVenue])
getVolumeByVenue ticker = do
  obj <- getNonJSONData (baseURL ++ lowerString ticker ++ "/volume-by-venue")
  return $ decode obj

getNonJSONData :: String -> IO L8.ByteString
getNonJSONData query = do
    obj <- simpleHttp query
    return obj
