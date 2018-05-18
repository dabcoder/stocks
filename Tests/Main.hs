module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import Test.HUnit
import Net.Stocks
import Data.Maybe

main :: IO Counts
main = runTestTT tests

tests = TestList [TestLabel "" testChart,
                  TestLabel "" testCompany,
                  TestLabel "" testDelayedQuote,
                  TestLabel "" testDelayedDividend,
                  TestLabel "" testEarnings,
                  TestLabel "" testEffectiveSpread,
                  TestLabel "" testFinancials,
                  TestLabel "" testStats,
                  TestLabel "" testNewsItem,
                  TestLabel "" testOHLC,
                  TestLabel "" testPeers,
                  TestLabel "" testPrevious,
                  TestLabel "" testPrice,
                  TestLabel "" testQuote,
                  TestLabel "" testSplit,
                  TestLabel "" testVolumeByVenue,
                  TestLabel "" testTypeQuery1,
                  TestLabel "" testBatch,
                  TestLabel "" testMultiBatch1,
                  TestLabel "" testMultiBatch2]

testChart = TestCase (do result <- getChart "aapl"
                         assertBool "desc" (result /= Nothing))
testCompany = TestCase (do result <- getCompany "aapl"
                           assertBool "desc" (result /= Nothing))
testDelayedQuote = TestCase (do result <- getDelayedQuote "aapl"
                                assertBool "desc" (result /= Nothing))
testDelayedDividend = TestCase (do result <- getDelayedDividend "aapl"
                                   assertBool "desc" (result /= Nothing))
testEarnings = TestCase (do result <- getEarnings "aapl"
                            assertBool "desc" (result /= Nothing))
testEffectiveSpread = TestCase (do result <- getEffectiveSpread "aapl"
                                   assertBool "desc" (result /= Nothing))
testFinancials = TestCase (do result <- getFinancials "aapl"
                              assertBool "desc" (result /= Nothing))
testStats = TestCase (do result <- getStats "aapl"
                         assertBool "desc" (result /= Nothing))
testNewsItem = TestCase (do result <- getNewsItem "aapl"
                            assertBool "desc" (result /= Nothing))
testOHLC = TestCase (do result <- getOHLC "aapl"
                        assertBool "desc" (result /= Nothing))
testPeers = TestCase
  (do result <- getPeers "aapl"
      assertEqual "get the correct peers for AAPL" result
        (L8.pack "[\"MSFT\",\"NOK\",\"IBM\",\"HPQ\",\"GOOGL\",\"BB\",\"XLK\"]"))
testPrevious = TestCase (do result <- getPrevious "aapl"
                            assertBool "desc" (result /= Nothing))
testPrice = TestCase (do result <- getPrice "aapl"
                         assertBool "" (result /= Nothing))
testQuote = TestCase (do result <- getQuote "aapl"
                         assertBool "desc" (result /= Nothing))
testSplit = TestCase (do result <- getSplit "aapl"
                         assertBool "desc" (result /= Nothing))
testVolumeByVenue = TestCase (do result <- getVolumeByVenue "aapl"
                                 assertBool "desc" (result /= Nothing))
testTypeQuery1 = TestCase (assertEqual "" "types=news,ohlc"
                           (typeQuery [NewsQuery, OHLCQuery]))

-- FIXME: when record names can be uniquely defined, we need to
--        have more specific tests, i.e. not just /= Nothing

-- the following batch requests asks for stats, news, company:
-- getBatchCompany "aapl" [StatsQuery, NewsQuery, CompanyQuery]
-- testBatch = TestCase
--   (do result <- getBatchCompany "aapl" [StatsQuery, NewsQuery, CompanyQuery]
--       assertEqual "" (pickCompany result) "Apple Inc.")

-- pickCompanyName :: Maybe Batch -> String
-- pickCompanyName (Just (Batch {stats = st})) = pickCompanyName' st

-- pickCompanyName' :: Maybe Company -> String
-- pickCompanyName' (Just (Stats {companyName = cname})) = fromJust cname

testBatch = TestCase
  (do result <- getBatchCompany "aapl" [StatsQuery, NewsQuery, CompanyQuery]
      assertBool "" (result /= Nothing))

testMultiBatch1 = TestCase
  (do result <- getBatch ["fb"] [OHLCQuery]
      assertBool "" (result /= Nothing))

testMultiBatch2 = TestCase
  (do result <- getBatch ["dps", "fb"] [OHLCQuery, CompanyQuery]
      assertBool "" (result /= Nothing))
