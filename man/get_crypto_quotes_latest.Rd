% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Coinmarketcap_API.R
\name{get_crypto_quotes_latest}
\alias{get_crypto_quotes_latest}
\title{Retrieve the latest quotes from Cryptocurrencies}
\usage{
get_crypto_quotes_latest(
  currency = "USD",
  symbol = NULL,
  slug = NULL,
  id = NULL
)
}
\arguments{
\item{currency}{At what currency should the prices be converted}

\item{symbol}{Cryptocurrencies symbols}

\item{slug}{Cryptocurrencies slugs}

\item{id}{Cryptocurrencies id's}
}
\description{
Retrieve the latest quotes from Cryptocurrencies
}
\seealso{
Other CMC API: 
\code{\link{get_crypto_dim_map}()},
\code{\link{get_crypto_listings_latest}()}
}
\concept{CMC API}
