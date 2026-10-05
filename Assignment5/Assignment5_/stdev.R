# stdev.R
# Standard Deviation (population form, divides by n)
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# Arguments:
#   data numeric vector of values
# Returns:
#   single numeric value, the population standard deviation of data

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- numeric(length(data))
  for (i in seq_along(data)) {
    diff_values[i] <- data[i] - mean_value
  }

  # Calculate the squared differences
  squared_diff <- numeric(length(diff_values))
  for (i in seq_along(diff_values)) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
