#########################################################################
#
# SuperTrend implemented in R by Lars Bernhardsson 12/14/2021.
#
# Main algorithm translated from Python located at:
#
#    https://github.com/hackingthemarkets/supertrend-crypto-bot
#
#########################################################################


SuperTrendCombo <- function(x) {
  stopifnot(xts::xtsible(x))
  stopifnot(xts::is.xts(x <- xts::as.xts(x)))
  stopifnot(quantmod::is.OHLC(x <- quantmod::OHLC(x)))

  combined <- SuperTrend(x = x, atr.period = 10, atr.multiplier = 3)
  st_long <- SuperTrend(x = x, atr.period = 20, atr.multiplier = 5)

  combined$st_position <- FALSE

  combined$st_signal <- ifelse(
    combined$st_signal == 1 & st_long$st_uptrend,
    1,
    ifelse(
      combined$st_signal == -1 & !st_long$st_uptrend,
      -1,
      0
    )
  )

  for (current in 2:nrow(x)) {
    previous <- current - 1

    nxt <- ifelse(
      current == nrow(x),
      NA,
      current + 1
    )

    if (combined$st_signal[current] == 1 & combined$st_signal[previous] != 1 & !is.na(nxt)) {
      combined$st_position[nxt] <- TRUE
    }

    if (combined$st_signal[current] == -1 & combined$st_signal[previous] != -1 & !is.na(nxt)) {
      combined$st_position[nxt] <- FALSE
    }
  }

  return(combined[, c("st_uband", "st_lband", "st_uptrend", "st_signal", "st_position")])
}
