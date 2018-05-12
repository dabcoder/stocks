{-# LANGUAGE RecordWildCards #-}

module Main where

import Test.HUnit
import Net.Stocks

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
                  TestLabel "" testPrevious,
                  -- TestLabel "" testPrice, -- FIXME: how to test?
                  TestLabel "" testQuote,
                  TestLabel "" testSplit,
                  TestLabel "" testVolumeByVenue]

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
testPrevious = TestCase (do result <- getPrevious "aapl"
                            assertBool "desc" (result /= Nothing))
testQuote = TestCase (do result <- getQuote "aapl"
                         assertBool "desc" (result /= Nothing))
testSplit = TestCase (do result <- getSplit "aapl"
                         assertBool "desc" (result /= Nothing))
testVolumeByVenue = TestCase (do result <- getVolumeByVenue "aapl"
                                 assertBool "desc" (result /= Nothing))
