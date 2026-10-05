# crossover.R
# Crossover detection between two series
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# Arguments:
#   arr1 numeric vector (for example a fast moving average)
#   arr2 numeric vector of the same length (for example a slow moving average)
# Returns:
#   character vector of the same length as arr1:
#     "Up"   arr1 crossed above arr2 at that point
#     "Down" arr1 crossed below arr2 at that point
#     "None" no crossing at that point

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"

  # Check for crossovers at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}
