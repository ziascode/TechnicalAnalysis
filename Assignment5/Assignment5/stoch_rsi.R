# stoch_rsi.R
# Stochastic RSI (StochRSI)
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# Arguments:
#   data     numeric vector of values (for example closing prices)
#   period   integer, period used for the RSI calculation
#   k_period integer, smoothing period for the %K line
#   d_period integer, smoothing period for the %D line
# Returns:
#   list with k_line and d_line
#
# Depends on: rsi() from rsi.R and sma() from sma.R

# Load rsi() and sma() if they have not been defined yet
if (!exists("rsi", mode = "function")) {
  source("rsi.R")
}
if (!exists("sma", mode = "function")) {
  source("sma.R")
}

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI
  # NA entries (the warm-up portion of the RSI) are left out of the min and max
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  if (length(valid_rsi) == 0) {
    # Not enough data to produce any RSI value, so StochRSI is undefined
    k_values <- rep(NA_real_, length(rsi_values))
  } else {
    min_rsi <- min(valid_rsi)
    max_rsi <- max(valid_rsi)
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}
