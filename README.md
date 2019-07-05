# stocks

# !!! Legacy iex API client. For up to date client please see [iexcloud](https://github.com/ksallberg/iexcloud).

[![Build Status](https://travis-ci.org/dabcoder/stocks.svg?branch=master)](https://travis-ci.org/dabcoder/stocks)  

Haskell library for the IEX trading API.  

Example:

```haskell
stack build && stack ghci

> getCompany "aapl"

Just (Company {symbol = "AAPL",
               companyName = "Apple Inc.",
               exchange = "Nasdaq Global Select",
               industry = "Computer Hardware",
               website = "http://www.apple.com",
               description = "Apple Inc is designs ...",
               ceo = "Timothy D. Cook",
               issueType = "cs",
               sector = "Technology"})

> getPrice "dps"

Just 120.36
```

Please see the HUnit test for a complete example
of all API calls.

## How to run test suite
```
stack test
```

## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/dabcoder/stocks/issues).

### Attribution
If you redistribute our API data:

* Cite IEX using the following text and link: “Data provided for free by [IEX](https://iextrading.com/developer).”
* Provide a link to https://iextrading.com/api-exhibit-a in your terms of service.
