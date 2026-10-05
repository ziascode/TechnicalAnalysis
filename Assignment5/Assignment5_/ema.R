# ema.R
# Exponential Moving Average (EMA)
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# Arguments:
#   data   numeric vector of values (for example closing prices)
#   period integer, number of data points used for the EMA calculation
# Returns:
#   numeric vector of the same length as data

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
