# macd.R
# Moving Average Convergence Divergence (MACD)
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# Arguments:
#   data          numeric vector of values (for example closing prices)
#   short_period  integer, period of the short-term EMA
#   long_period   integer, period of the long-term EMA
#   signal_period integer, period of the signal line EMA
# Returns:
#   list with macd_line, signal_line and histogram (each the same length as data)
#
# Depends on: ema() from ema.R

# Load ema() if it has not been defined yet
if (!exists("ema", mode = "function")) {
  source("ema.R")
}

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}
