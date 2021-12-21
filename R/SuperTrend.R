#########################################################################
#
# SuperTrend with extensions implemented by Lars Bernhardsson 12/14/2021.
#
# Main algorithm translated from Python located at:
#
#    https://github.com/hackingthemarkets/supertrend-crypto-bot
#
# Current extensions:
#    Adding a stop buy/sell based on RSI.
#
#########################################################################


SuperTrend <- function(x,
                       rsi.period = 14,
                       buy.rsi.stop = 60,
                       sell.rsi.stop = 40,
                       atr.period = 7,
                       atr.multiplier = 3) {
  stopifnot(xts::xtsible(x))
  stopifnot(xts::is.xts(x <- xts::as.xts(x)))
  stopifnot(is.numeric(buy.rsi.stop <- as.numeric(buy.rsi.stop)))
  stopifnot(is.numeric(sell.rsi.stop <- as.numeric(sell.rsi.stop)))
  stopifnot(is.numeric(period <- as.numeric(period)))
  stopifnot(is.numeric(atr.multiplier <- as.numeric(atr.multiplier)))
  stopifnot(is.numeric(sim.start <- as.numeric(sim.start)))
  stopifnot(isTRUE(sim.start > 1) & isTRUE(sim.start < nrow(x)))
  stopifnot(quantmod::is.OHLC(x <- quantmod::OHLC(x)))

  x <- merge(x, TTR::RSI(x[, 4], n = rsi.period))
  x <- merge(x, TTR::ATR(x, n = atr.period))

  hl2 <- (x[, 2] + x[, 3]) / 2

  x$st_uband <- hl2 + (atr.multiplier * x$atr)
  x$st_lband <- hl2 - (atr.multiplier * x$atr)
  x$st_uptrend <- TRUE
  x$st_signal <- 0
  x$st_position <- FALSE

  for (current in 2:nrow(x)) {
    previous <- current - 1

    nxt <- ifelse(
      current == nrow(x),
      NA,
      current + 1
    )

    if (isTRUE(as.numeric(x[current, 4]) > as.numeric(x$st_uband[previous]))) {
      x$st_uptrend[current] <- TRUE
    } else if (isTRUE(as.numeric(x[current, 4]) < as.numeric(x$st_lband[previous]))) {
      x$st_uptrend[current] <- FALSE
    } else {
      x$st_uptrend[current] <- x$st_uptrend[previous]

      if (isTRUE(as.logical(x$st_uptrend[current])) & isTRUE((as.numeric(x$st_lband[current]) < as.numeric(x$st_lband[previous])))) {
        x$st_lband[current] <- x$st_lband[previous]
      }

      if (isTRUE(as.logical(!x$st_uptrend)) & isTRUE((as.numeric(x$st_uband[current]) > as.numeric(x$st_uband[previous])))) {
        x$st_uband[current] <- x$st_uband[previous]
      }
    }

    if (isTRUE(as.logical(!x$st_uptrend[previous])) & isTRUE(as.logical(x$st_uptrend[current])) & isTRUE(as.numeric(x$rsi[current]) < buy.rsi.stop)) {
      x$st_signal[current] <- 1

      if (!is.na(nxt)) {
        x$st_position[nxt:nrow(x)] <- TRUE
      }
    }

    if (isTRUE(as.logical(x$st_uptrend[previous])) & isTRUE(as.logical(!x$st_uptrend[current])) & isTRUE(as.numeric(x$rsi[current]) > sell.rsi.stop)) {
      x$st_signal[current] <- -1

      if (!is.na(nxt)) {
        x$st_position[nxt:nrow(x)] <- FALSE
      }
    }
  }

  return(x[, c("st_uband", "st_lband", "st_uptrend", "st_signal", "st_position")])
}
