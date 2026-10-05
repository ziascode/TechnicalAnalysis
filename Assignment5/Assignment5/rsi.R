# rsi.R
# Relative Strength Index (RSI) using Wilder's smoothing method
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# Arguments:
#   data   numeric vector of values (for example closing prices)
#   period integer, number of data points used for the RSI calculation
# Returns:
#   numeric vector of the same length as data. The first 'period' entries
#   are NA because there is not enough history to compute an RSI for them.

rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses
  # (mean of the first 'period' elements)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))

  # Calculate RSI values using the Wilder's smoothing method
  # The guard stops R from counting backwards when data is not longer than period
  if (length(data) > period) {
    for (i in (period + 1):length(data)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
