{-# LANGUAGE RecordWildCards #-}

import Data.List.NonEmpty as NE
import Net.Stocks

comp :: String
comp = "AAPL"

main :: IO ()
main = do
    resp <- getData comp QueryFinancials
    case resp of
        Nothing -> putStrLn "No financial data for that company"
        Just (FinancialsList l) -> do
            putStrLn $ " value: " ++ (show $ grossProfit (NE.head l))