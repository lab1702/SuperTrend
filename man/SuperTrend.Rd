\name{SuperTrend}
\alias{SuperTrend}
\title{SuperTrend}
\usage{
SuperTrend(
  df,
  buy.rsi.stop = 60,
  sell.rsi.stop = 40,
  period = 7,
  atr.multiplier = 3,
  sim.start = 2
)
}
\description{
Caldulates the SuperTrend indicator based on OHLC data.
}
\examples{
SuperTrend(d)
}
