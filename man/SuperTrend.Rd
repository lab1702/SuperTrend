\name{SuperTrend}
\alias{SuperTrend}
\title{SuperTrend}
\usage{
SuperTrend(
  df,
  rsi.period = 14,
  buy.rsi.stop = 60,
  sell.rsi.stop = 40,
  atr.period = 7,
  atr.multiplier = 3
)
}
\description{
Caldulates the SuperTrend indicator based on OHLC data.
}
\examples{
SuperTrend(d)
}
