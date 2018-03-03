# stocks  

Haskell library for the IEX trading API.  

Example:  

```haskell
{-# LANGUAGE RecordWildCards #-}

import Net.Stocks

comp :: String
comp = "AAPL"

main :: IO ()
main = do
    resp <- getData comp QueryStocks
    case resp of
        Nothing -> putStrLn "No data for that company"
        Just (Stock{..}) -> do
            putStrLn $ "Stock value: " ++ show latestPrice
```

Which should show:  

```
Stock value: <the_actual_stock_value>
```
