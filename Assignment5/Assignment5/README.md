# Assignment 5: Technical Analysis using R, Development Phase

BDA400 Data Science Tools and Techniques

This folder holds the nine technical indicator functions for the second stage of the technical analysis project. Every function is written with core R only. No packages are loaded.

## Files

| File | Function | Purpose |
|---|---|---|
| sma.R | `sma(data, period)` | Simple Moving Average |
| ema.R | `ema(data, period)` | Exponential Moving Average |
| macd.R | `macd(data, short_period, long_period, signal_period)` | MACD line, signal line, histogram |
| stdev.R | `stdev(data)` | Population standard deviation |
| linreg.R | `linreg(regressionSource, regressionLength, regressionOffset)` | Linear regression slope, intercept, predicted values |
| rsi.R | `rsi(data, period)` | Relative Strength Index with Wilder smoothing |
| stoch_rsi.R | `stoch_rsi(data, period, k_period, d_period)` | Stochastic RSI %K and %D lines |
| crossover.R | `crossover(arr1, arr2)` | Up / Down / None crossing signals |
| crossunder.R | `crossunder(arr1, arr2)` | Crossunder signals |
| run_examples.R | | Runs every handout example and 42 correctness checks |

## How to run

From a terminal:

```
cd TechnicalAnalysis/Assignment5
Rscript run_examples.R
```

From RStudio:

```r
setwd("~/TechnicalAnalysis/Assignment5")
source("run_examples.R")
```

To use one indicator on its own:

```r
setwd("~/TechnicalAnalysis/Assignment5")
source("sma.R")
sma(c(10, 12, 15, 20, 18, 22, 25, 24, 21), period = 3)
```

`macd.R` needs `ema.R`, and `stoch_rsi.R` needs `rsi.R` and `sma.R`. Each of those files loads what it needs automatically when the working directory is this folder.
