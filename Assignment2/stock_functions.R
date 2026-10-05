# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
# Student: Zia Yousaf
# File:    stock_functions.R
# Purpose: Utility functions to read the portfolio file, import stock data
#          with quantmod, and compute basic statistics for each stock.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Package setup
# ------------------------------------------------------------------------------
# Installs any package that is missing, then loads all of them.
setup_packages <- function(packages = c("quantmod", "TTR", "xts", "zoo")) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.rstudio.com/")
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(packages)
}

setup_packages()

# ------------------------------------------------------------------------------
# 2. Read the portfolio file
# ------------------------------------------------------------------------------
# Reads 'portfolio.txt' (one stock symbol per line) and returns a clean
# character vector of symbols: trimmed, upper case, no blanks, no duplicates.
read_portfolio <- function(portfolio_file = "portfolio.txt") {
  if (!file.exists(portfolio_file)) {
    stop("Portfolio file not found: ", portfolio_file,
         "\nSet the working directory to the folder that contains it.")
  }
  symbols <- readLines(portfolio_file, warn = FALSE)
  symbols <- toupper(trimws(symbols))
  symbols <- symbols[symbols != ""]
  symbols <- unique(symbols)
  if (length(symbols) == 0) {
    stop("The portfolio file is empty: ", portfolio_file)
  }
  symbols
}

# ------------------------------------------------------------------------------
# 3. Import and load stock data
# ------------------------------------------------------------------------------
# Reads the portfolio file and downloads daily price history for every symbol
# from Yahoo Finance using quantmod::getSymbols().
#
# Arguments:
#   portfolio_file  path to the text file with one symbol per line
#   from, to        date range to download (default: the last 365 days)
#   assign_global   if TRUE, each stock is also saved in the global environment
#                   as its own data frame named <SYMBOL>_df (e.g. AAPL_df)
#
# Returns: a named list with one data frame per stock symbol. Each data frame
#          has the columns Date, Open, High, Low, Close, Volume, Adjusted.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date(),
                            assign_global = TRUE) {
  symbols <- read_portfolio(portfolio_file)
  cat("Symbols found in", portfolio_file, ":",
      paste(symbols, collapse = ", "), "\n")

  stock_list <- list()

  for (sym in symbols) {
    cat("Downloading", sym, "... ")

    raw <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE, warnings = FALSE),
      error = function(e) {
        cat("FAILED (", conditionMessage(e), ")\n", sep = "")
        NULL
      }
    )
    if (is.null(raw)) next

    # Convert the xts object returned by quantmod into a regular data frame
    df <- data.frame(Date = as.Date(index(raw)),
                     coredata(raw),
                     row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

    # Remove days with missing prices (holidays, incomplete rows)
    df <- df[complete.cases(df[, c("Open", "High", "Low", "Close")]), ]

    stock_list[[sym]] <- df

    if (assign_global) {
      df_name <- paste0(make.names(sym), "_df")
      assign(df_name, df, envir = .GlobalEnv)
    }

    cat("OK (", nrow(df), " rows)\n", sep = "")
  }

  if (length(stock_list) == 0) {
    stop("No stock data could be downloaded. Check the internet connection ",
         "and the symbols in ", portfolio_file, ".")
  }

  stock_list
}

# ------------------------------------------------------------------------------
# 4. Statistics helpers
# ------------------------------------------------------------------------------
# Base R has no function for the statistical mode, so one is defined here.
# Daily prices rarely repeat to the cent, so prices are rounded first
# (default: nearest whole dollar) and the most frequent value is returned.
# If several values tie, the smallest one is returned.
calculate_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  if (length(x) == 0) return(NA_real_)
  freq <- table(x)
  as.numeric(names(freq)[which.max(freq)])
}

# ------------------------------------------------------------------------------
# 5. Compute basic statistics for one stock
# ------------------------------------------------------------------------------
# Takes one stock's data frame and calculates the required statistics on the
# closing price: moving average, mean, mode, median and standard deviation.
#
# Arguments:
#   stock_df    data frame produced by load_stock_data()
#   symbol      stock symbol (used for labelling)
#   short_n     window of the short moving average (default 20 days)
#   long_n      window of the long moving average (default 50 days)
#
# Returns: a list with
#   summary   one-row data frame with the statistics
#   data      the input data frame plus the moving average columns
calculate_statistics <- function(stock_df, symbol = "",
                                 short_n = 20, long_n = 50) {
  close <- stock_df$Close
  n <- length(close)

  # Simple moving averages from the TTR package. A moving average needs at
  # least as many rows as its window, otherwise NA is returned.
  sma_short <- if (n >= short_n) SMA(close, n = short_n) else rep(NA_real_, n)
  sma_long  <- if (n >= long_n)  SMA(close, n = long_n)  else rep(NA_real_, n)

  data_out <- stock_df
  data_out[[paste0("SMA", short_n)]] <- round(sma_short, 2)
  data_out[[paste0("SMA", long_n)]]  <- round(sma_long, 2)

  summary_df <- data.frame(
    Symbol = symbol,
    Days   = n,
    Mean   = round(mean(close, na.rm = TRUE), 2),
    Median = round(median(close, na.rm = TRUE), 2),
    Mode   = calculate_mode(close),
    StdDev = round(sd(close, na.rm = TRUE), 2),
    Min    = round(min(close, na.rm = TRUE), 2),
    Max    = round(max(close, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
  summary_df[[paste0("SMA", short_n)]] <- round(sma_short[n], 2)
  summary_df[[paste0("SMA", long_n)]]  <- round(sma_long[n], 2)

  list(summary = summary_df, data = data_out)
}

# ------------------------------------------------------------------------------
# 6. Compute statistics for the whole portfolio
# ------------------------------------------------------------------------------
# Applies calculate_statistics() to every stock in the list.
#
# Returns: a list with
#   summary   data frame with one row of statistics per stock
#   details   named list of data frames (prices plus moving averages)
calculate_portfolio_statistics <- function(stock_list,
                                           short_n = 20, long_n = 50) {
  results <- lapply(names(stock_list), function(sym) {
    calculate_statistics(stock_list[[sym]], sym, short_n, long_n)
  })
  names(results) <- names(stock_list)

  summary_table <- do.call(rbind, lapply(results, function(r) r$summary))
  rownames(summary_table) <- NULL

  list(summary = summary_table,
       details = lapply(results, function(r) r$data))
}
