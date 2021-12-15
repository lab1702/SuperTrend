###########################################################################
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
###########################################################################


SuperTrend <- function(df,
                       buy.rsi.stop = 60,
                       sell.rsi.stop = 40,
                       period = 7,
                       atr.multiplier = 3,
                       sim.start = 2) {
  stopifnot(xts::xtsible(df))
  stopifnot(xts::is.xts(df <- xts::as.xts(df)))
  stopifnot(is.numeric(period <- as.numeric(period)))
  stopifnot(is.numeric(atr.multiplier <- as.numeric(atr.multiplier)))
  stopifnot(is.numeric(sim.start <- as.numeric(sim.start)))
  stopifnot(isTRUE(sim.start > 1) & isTRUE(sim.start < nrow(df)))
  stopifnot(quantmod::is.HLC(df <- quantmod::HLC(df)))

  df <- merge(df, TTR::RSI(df[, 3]))
  df <- merge(df, TTR::ATR(df, n = period))

  hla <- (df[, 1] + df[, 2]) / 2

  df$upperband <- hla + (atr.multiplier * df$atr)
  df$lowerband <- hla - (atr.multiplier * df$atr)
  df$in_uptrend <- TRUE
  df$signal <- 0
  df$in_pos <- FALSE

  for (current in sim.start:nrow(df)) {
    previous <- current - 1

    nxt <- ifelse(
      current == nrow(df),
      NA,
      current + 1
    )

    if (isTRUE(as.numeric(df[current, 3]) > as.numeric(df$upperband[previous]))) {
      df$in_uptrend[current] <- TRUE
    } else if (isTRUE(as.numeric(df[current, 3]) < as.numeric(df$lowerband[previous]))) {
      df$in_uptrend[current] <- FALSE
    } else {
      df$in_uptrend[current] <- df$in_uptrend[previous]

      if (isTRUE(as.logical(df$in_uptrend[current])) & isTRUE((as.numeric(df$lowerband[current]) < as.numeric(df$lowerband[previous])))) {
        df$lowerband[current] <- df$lowerband[previous]
      }

      if (isTRUE(as.logical(!df$in_uptrend)) & isTRUE((as.numeric(df$upperband[current]) > as.numeric(df$upperband[previous])))) {
        df$upperband[current] <- df$upperband[previous]
      }
    }

    if (isTRUE(as.logical(!df$in_uptrend[previous])) & isTRUE(as.logical(df$in_uptrend[current])) & isTRUE(as.numeric(df$rsi[current]) < buy.rsi.stop)) {
      df$signal[current] <- 1

      if (!is.na(nxt)) {
        df$in_pos[nxt:nrow(df)] <- TRUE
      }
    }

    if (isTRUE(as.logical(df$in_uptrend[previous])) & isTRUE(as.logical(!df$in_uptrend[current])) & isTRUE(as.numeric(df$rsi[current]) > sell.rsi.stop)) {
      df$signal[current] <- -1

      if (!is.na(nxt)) {
        df$in_pos[nxt:nrow(df)] <- FALSE
      }
    }
  }

  df[, c("upperband", "lowerband", "in_uptrend", "signal", "in_pos")]
}
