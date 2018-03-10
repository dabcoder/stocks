module StocksSpec where

import Test.Hspec
import Net.Stocks

main :: IO ()
main = hspec $ do
    -- quote URL
    describe "stocksQuery" $ do
        it "returns the URL with the quote endpoint" $
            stocksQuery "AAPL" `shouldBe` "https://api.iextrading.com/1.0/stock/AAPL/quote"
    -- financials URL
    describe "financialsQuery" $ do
        it "returns the URL with the financials endpoint" $
            financialsQuery "AAPL" `shouldBe` "https://api.iextrading.com/1.0/stock/AAPL/financials"
    -- peers URL
    describe "peersQuery" $ do
        it "returns the URL with the peers endpoint" $
            peersQuery "AAPL" `shouldBe` "https://api.iextrading.com/1.0/stock/AAPL/peers"
    -- price URL
    describe "priceQuery" $ do
        it "returns the URL with the price endpoint" $
            priceQuery "AAPL" `shouldBe` "https://api.iextrading.com/1.0/stock/AAPL/price"