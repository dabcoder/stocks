# stocks

[![Build Status](https://travis-ci.org/dabcoder/stocks.svg?branch=master)](https://travis-ci.org/dabcoder/stocks)

Haskell library for the IEX trading API.

Example:

```haskell
stack build && stack ghci

getQuote :: AuthAndSymbol -> IO (Maybe IEXQuote.Quote)
> getQuote ("pk_myAPItoken", "msft")

Just (Quote {symbol = "MSFT",
             companyName = "Microsoft Corp.",
             primaryExchange = Nothing,
             sector = Nothing,
             calculationPrice = "tops",
             open = 128.9,
             ...
             })

getCompany :: AuthAndSymbol -> IO (Maybe IEXCompany.Company)
> getCompany ("pk_myAPItoken", "aapl")

Just (Company {symbol = "AAPL",
               companyName = "Apple Inc.",
               exchange = "Nasdaq Global Select",
               industry = "Computer Hardware",
               website = "http://www.apple.com",
               description = "Apple Inc is designs ...",
               ceo = "Timothy D. Cook",
               issueType = "cs",
               sector = "Technology"})

getPrice :: AuthAndSymbol -> IO (Maybe Double)
> getPrice ("pk_myAPItoken", "msft")

Just 131.56
```

Please see the HUnit test for a complete example
of all API calls.

## How to run test suite
```
stack test
```

## Contribute

For any problems, comments, or feedback please create an
issue [here on GitHub](https://github.com/dabcoder/stocks/issues).

### Attribution
If you redistribute our API data:

* Cite IEX using the following text and link: “Data provided for free by [IEX](https://iextrading.com/developer).”
* Provide a link to https://iextrading.com/api-exhibit-a in your terms of service.
