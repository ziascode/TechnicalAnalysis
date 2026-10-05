# run_examples.R
# Runs every indicator with the example inputs from the assignment handout
# and then checks each result against an independent calculation.
# BDA400 Assignment 5: Technical Analysis using R, Development Phase
#
# How to run (working directory must be this folder):
#   Rscript run_examples.R
# or, inside RStudio:
#   setwd("path/to/TechnicalAnalysis/Assignment5"); source("run_examples.R")

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

section <- function(title) {
  cat("\n==================================================\n")
  cat(title, "\n")
  cat("==================================================\n")
}

# ---------------------------------------------------------------
# Part 1: examples from the assignment handout
# ---------------------------------------------------------------

section("1. sma(data, period = 3)")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
sma_result <- sma(data, period = 3)
print(sma_result)

section("2. ema(data, period = 3)")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
ema_result <- ema(data, period = 3)
print(ema_result)

section("3. macd(data, short_period = 3, long_period = 5, signal_period = 2)")
data <- c(100, 105, 110, 115, 120, 125, 130)
macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
print(macd_result)

section("4. stdev(data)")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
stdev_result <- stdev(data)
print(stdev_result)

section("5. linreg(data, regressionLength = 5, regressionOffset = 0)")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
linreg_result <- linreg(data, regressionLength = 5, regressionOffset = 0)
print(linreg_result)

section("6. rsi(data, period = 5)")
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
rsi_result <- rsi(data, period = 5)
print(rsi_result)

section("7a. stoch_rsi(data, period = 14, k_period = 3, d_period = 3)")
cat("Handout example. The series has 10 points and the RSI period is 14,\n")
cat("so no RSI value can be formed and every output is NA.\n\n")
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
stoch_rsi_result <- stoch_rsi(data, period = 14, k_period = 3, d_period = 3)
print(stoch_rsi_result)

section("7b. stoch_rsi(prices, period = 5, k_period = 3, d_period = 3)")
cat("Same function on a 20 point series so that real values are produced.\n\n")
prices <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62,
            59, 61, 66, 70, 68, 64, 63, 67, 72, 71)
stoch_rsi_long <- stoch_rsi(prices, period = 5, k_period = 3, d_period = 3)
print(stoch_rsi_long)

section("8. crossover(arr1, arr2)")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
crossover_signals <- crossover(arr1, arr2)
print(crossover_signals)

section("9. crossunder(arr1, arr2)")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
crossunder_signals <- crossunder(arr1, arr2)
print(crossunder_signals)
cat("\nSecond pair, where arr1 falls below arr2 at position 4:\n")
print(crossunder(c(20, 19, 18, 12, 11, 15), c(15, 15, 15, 15, 15, 15)))

# ---------------------------------------------------------------
# Part 2: correctness checks
# ---------------------------------------------------------------

section("Correctness checks")

passed <- 0
failed <- 0
check <- function(label, condition) {
  if (isTRUE(condition)) {
    passed <<- passed + 1
    cat(sprintf("PASS  %s\n", label))
  } else {
    failed <<- failed + 1
    cat(sprintf("FAIL  %s\n", label))
  }
}
raises <- function(expr) {
  inherits(try(expr, silent = TRUE), "try-error")
}

d9 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# SMA
check("sma: matches hand calculated window means",
      isTRUE(all.equal(sma(d9, 3),
                       c(37, 47, 53, 60, 65, 71, 70) / 3)))
check("sma: output length is n - period + 1", length(sma(d9, 3)) == 7)
check("sma: period equal to length returns the overall mean",
      isTRUE(all.equal(sma(d9, 9), mean(d9))))
check("sma: error when data is shorter than period", raises(sma(c(1, 2), 3)))

# EMA
check("ema: first value equals first data point", ema(d9, 3)[1] == 10)
check("ema: matches hand calculation (multiplier 0.5)",
      isTRUE(all.equal(ema(d9, 3)[1:4], c(10, 11, 13, 16.5))))
check("ema: output length equals input length", length(ema(d9, 3)) == 9)
check("ema: period 1 reproduces the data", isTRUE(all.equal(ema(d9, 1), d9)))

# MACD
m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2)
check("macd: returns macd_line, signal_line, histogram",
      identical(names(m), c("macd_line", "signal_line", "histogram")))
check("macd: macd_line equals short EMA minus long EMA",
      isTRUE(all.equal(m$macd_line,
                       ema(c(100, 105, 110, 115, 120, 125, 130), 3) -
                         ema(c(100, 105, 110, 115, 120, 125, 130), 5))))
check("macd: signal_line equals EMA of macd_line",
      isTRUE(all.equal(m$signal_line, ema(m$macd_line, 2))))
check("macd: histogram equals macd_line minus signal_line",
      isTRUE(all.equal(m$histogram, m$macd_line - m$signal_line)))
check("macd: positive in a steady uptrend", all(m$macd_line[-1] > 0))

# Standard deviation
n9 <- length(d9)
check("stdev: matches population standard deviation",
      isTRUE(all.equal(stdev(d9), sd(d9) * sqrt((n9 - 1) / n9))))
check("stdev: constant data gives zero", stdev(c(5, 5, 5, 5)) == 0)
check("stdev: known value, c(2,4,4,4,5,5,7,9) gives 2",
      isTRUE(all.equal(stdev(c(2, 4, 4, 4, 5, 5, 7, 9)), 2)))

# Linear regression
lr <- linreg(d9, 5, 0)
sub <- d9[4:9]
fit <- lm(sub ~ seq_along(sub))
check("linreg: slope matches lm()",
      isTRUE(all.equal(lr$slope, unname(coef(fit)[2]))))
check("linreg: intercept matches lm()",
      isTRUE(all.equal(lr$intercept, unname(coef(fit)[1]))))
check("linreg: predicted values match lm() fitted values",
      isTRUE(all.equal(lr$predicted_values, unname(fitted(fit)))))
perfect <- linreg(c(3, 5, 7, 9, 11), 5, 0)
check("linreg: exact line y = 2x + 1 recovered",
      isTRUE(all.equal(c(perfect$slope, perfect$intercept), c(2, 1))))
check("linreg: error when regressionLength exceeds data length",
      raises(linreg(d9, 20, 0)))
check("linreg: error when regressionOffset >= regressionLength",
      raises(linreg(d9, 3, 3)))

# RSI
r <- rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), 5)
check("rsi: output length equals input length", length(r) == 10)
check("rsi: first 'period' values are NA", all(is.na(r[1:5])))
check("rsi: remaining values lie between 0 and 100",
      all(r[6:10] >= 0 & r[6:10] <= 100))
# Hand calculation for position 6: initial averages 2.4 and 1.6, then
# avg_gain = (2.4 * 4 + 0) / 5 = 1.92 and avg_loss = (1.6 * 4 + 3) / 5 = 1.88
check("rsi: first value matches hand calculation (50.52632)",
      isTRUE(all.equal(r[6], 100 - 100 / (1 + 1.92 / 1.88))))
check("rsi: all gains gives 100", all(rsi(1:10, 3)[4:10] == 100))
check("rsi: all losses gives 0", all(rsi(10:1, 3)[4:10] == 0))

# Stochastic RSI
s <- stoch_rsi(prices, 5, 3, 3)
check("stoch_rsi: returns k_line and d_line",
      identical(names(s), c("k_line", "d_line")))
check("stoch_rsi: k_line length is n - k_period + 1", length(s$k_line) == 18)
check("stoch_rsi: d_line length is length(k_line) - d_period + 1",
      length(s$d_line) == 16)
kv <- s$k_line[!is.na(s$k_line)]
check("stoch_rsi: %K values lie between 0 and 1", all(kv >= 0 & kv <= 1))
check("stoch_rsi: d_line equals sma of k_line",
      isTRUE(all.equal(s$d_line, sma(s$k_line, 3))))
check("stoch_rsi: series shorter than the RSI period returns all NA",
      all(is.na(stoch_rsi_result$k_line)) && all(is.na(stoch_rsi_result$d_line)))

# Crossover
a1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
a2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
check("crossover: handout example gives a single Up at position 4",
      identical(crossover(a1, a2),
                c("None", "None", "None", "Up", "None", "None", "None", "None", "None")))
check("crossover: detects Down",
      identical(crossover(c(5, 5, 1), c(3, 3, 3)), c("None", "None", "Down")))
check("crossover: output length equals input length",
      length(crossover(a1, a2)) == length(a1))
check("crossover: error on unequal lengths", raises(crossover(1:3, 1:4)))

# Crossunder
check("crossunder: handout example has no crossunder",
      identical(crossunder(a1, a2),
                c("None", rep("False", 8))))
check("crossunder: detects a crossunder at the right position",
      identical(crossunder(c(20, 19, 18, 12, 11, 15), rep(15, 6)),
                c("None", "False", "False", "True", "False", "False")))
check("crossunder: agrees with crossover 'Down' signals",
      identical(crossunder(a2, a1) == "True", crossover(a2, a1) == "Down"))
check("crossunder: error on unequal lengths", raises(crossunder(1:3, 1:4)))

cat(sprintf("\n%d checks passed, %d failed\n", passed, failed))
if (failed > 0) {
  stop("One or more correctness checks failed")
}
