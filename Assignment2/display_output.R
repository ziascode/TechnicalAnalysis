# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
# Student: Zia Yousaf
# File:    display_output.R
# Purpose: Utilities to display the imported stock data, the calculated
#          statistics and visualizations. Run this file to execute the
#          whole assignment from start to finish.
# Usage:   In RStudio, set the working directory to this folder
#          (Session > Set Working Directory > To Source File Location)
#          and click Source.
# ==============================================================================

source("stock_functions.R")

# Small helper to print a section heading in the console
print_header <- function(title) {
  line <- paste(rep("=", 78), collapse = "")
  cat("\n", line, "\n ", title, "\n", line, "\n", sep = "")
}

# ------------------------------------------------------------------------------
# 1. Display the loaded data frame of one stock
# ------------------------------------------------------------------------------
# Shows the size of the data frame, its date range, and the first and last
# n rows, similar to the Historical Data tab on Yahoo Finance.
display_stock_data <- function(stock_df, symbol, n = 5) {
  print_header(paste("HISTORICAL DATA:", symbol))
  cat("Rows:", nrow(stock_df), "  Columns:", ncol(stock_df), "\n")
  cat("Date range:", format(min(stock_df$Date)), "to",
      format(max(stock_df$Date)), "\n")

  shown <- stock_df
  num_cols <- sapply(shown, is.numeric)
  shown[num_cols] <- lapply(shown[num_cols], round, 2)

  cat("\nFirst", n, "rows:\n")
  print(head(shown, n), row.names = FALSE)
  cat("\nLast", n, "rows:\n")
  print(tail(shown, n), row.names = FALSE)
}

# ------------------------------------------------------------------------------
# 2. Display a quote summary for one stock
# ------------------------------------------------------------------------------
# Presents the latest trading day the way a quote page on Yahoo Finance does:
# last close, daily change, day range, 52-week range and volume.
display_quote_summary <- function(stock_df, symbol) {
  n <- nrow(stock_df)
  last <- stock_df[n, ]
  prev_close <- if (n > 1) stock_df$Close[n - 1] else NA
  change <- last$Close - prev_close
  change_pct <- change / prev_close * 100

  print_header(paste("QUOTE SUMMARY:", symbol, "as of", format(last$Date)))
  cat(sprintf("%-18s %12.2f\n", "Last Close:", last$Close))
  cat(sprintf("%-18s %+12.2f (%+.2f%%)\n", "Change:", change, change_pct))
  cat(sprintf("%-18s %12.2f\n", "Previous Close:", prev_close))
  cat(sprintf("%-18s %12.2f\n", "Open:", last$Open))
  cat(sprintf("%-18s %12s\n", "Day's Range:",
              sprintf("%.2f - %.2f", last$Low, last$High)))
  cat(sprintf("%-18s %12s\n", "Period Range:",
              sprintf("%.2f - %.2f", min(stock_df$Low), max(stock_df$High))))
  cat(sprintf("%-18s %12s\n", "Volume:",
              format(last$Volume, big.mark = ",", scientific = FALSE)))
  cat(sprintf("%-18s %12s\n", "Average Volume:",
              format(round(mean(stock_df$Volume)), big.mark = ",",
                     scientific = FALSE)))
}

# ------------------------------------------------------------------------------
# 3. Display the statistics table for the whole portfolio
# ------------------------------------------------------------------------------
display_statistics <- function(summary_table) {
  print_header("PORTFOLIO STATISTICS (based on daily closing price)")
  print(summary_table, row.names = FALSE)
  cat("\nMode is calculated on closing prices rounded to the nearest dollar.\n")
  cat("SMA columns show the latest value of each simple moving average.\n")
}

# ------------------------------------------------------------------------------
# 4. Visualizations
# ------------------------------------------------------------------------------
# 4a. Candlestick chart with volume and moving averages (quantmod)
plot_candlestick <- function(stock_df, symbol, short_n = 20, long_n = 50) {
  price_xts <- xts(stock_df[, c("Open", "High", "Low", "Close", "Volume")],
                   order.by = stock_df$Date)
  ta <- "addVo()"
  if (nrow(stock_df) >= short_n) {
    ta <- paste0(ta, ";addSMA(n = ", short_n, ", col = 'blue')")
  }
  if (nrow(stock_df) >= long_n) {
    ta <- paste0(ta, ";addSMA(n = ", long_n, ", col = 'red')")
  }
  chartSeries(price_xts, name = paste(symbol, "Candlestick Chart"),
              type = "candlesticks", theme = chartTheme("white"), TA = ta)
}

# 4b. Line chart of the closing price with the moving averages and the mean
plot_close_with_ma <- function(detail_df, symbol) {
  sma_cols <- grep("^SMA", names(detail_df), value = TRUE)
  y_range <- range(detail_df[, c("Close", sma_cols)], na.rm = TRUE)

  plot(detail_df$Date, detail_df$Close, type = "l", lwd = 2, col = "black",
       ylim = y_range, xlab = "Date", ylab = "Closing Price (USD)",
       main = paste(symbol, "Closing Price and Moving Averages"))
  grid()
  colours <- c("blue", "red")
  for (i in seq_along(sma_cols)) {
    lines(detail_df$Date, detail_df[[sma_cols[i]]], col = colours[i], lwd = 2)
  }
  abline(h = mean(detail_df$Close), col = "darkgreen", lty = 2)
  legend("topleft", bty = "n",
         legend = c("Close", sma_cols, "Mean"),
         col = c("black", colours[seq_along(sma_cols)], "darkgreen"),
         lty = c(1, rep(1, length(sma_cols)), 2), lwd = 2)
}

# 4c. Histogram of closing prices with mean, median and mode marked
plot_close_histogram <- function(stock_df, symbol) {
  close <- stock_df$Close
  hist(close, breaks = 30, col = "lightblue", border = "white",
       xlab = "Closing Price (USD)",
       main = paste(symbol, "Distribution of Closing Prices"))
  abline(v = mean(close), col = "red", lwd = 2)
  abline(v = median(close), col = "darkgreen", lwd = 2, lty = 2)
  abline(v = calculate_mode(close), col = "purple", lwd = 2, lty = 3)
  legend("topright", bty = "n", legend = c("Mean", "Median", "Mode"),
         col = c("red", "darkgreen", "purple"), lty = c(1, 2, 3), lwd = 2)
}

# 4d. Portfolio comparison: every stock rebased to 100 on the first day
plot_portfolio_comparison <- function(stock_list) {
  rebased <- lapply(stock_list, function(df) {
    data.frame(Date = df$Date, Value = df$Close / df$Close[1] * 100)
  })
  y_range <- range(sapply(rebased, function(d) range(d$Value)))
  x_range <- range(do.call(c, lapply(rebased, function(d) d$Date)))
  colours <- rainbow(length(rebased), v = 0.8)

  plot(x_range, y_range, type = "n", xlab = "Date",
       ylab = "Rebased Closing Price (start = 100)",
       main = "Portfolio Performance Comparison")
  grid()
  for (i in seq_along(rebased)) {
    lines(rebased[[i]]$Date, rebased[[i]]$Value, col = colours[i], lwd = 2)
  }
  abline(h = 100, lty = 2, col = "grey40")
  legend("topleft", bty = "n", legend = names(rebased), col = colours, lwd = 2)
}

# 4e. Bar chart comparing mean, median and mode across the portfolio
plot_statistics_bars <- function(summary_table) {
  values <- t(as.matrix(summary_table[, c("Mean", "Median", "Mode")]))
  colnames(values) <- summary_table$Symbol
  barplot(values, beside = TRUE, col = c("steelblue", "darkorange", "purple"),
          ylim = c(0, max(values, na.rm = TRUE) * 1.25),
          ylab = "Closing Price (USD)",
          main = "Mean, Median and Mode of Closing Price by Stock",
          legend.text = TRUE,
          args.legend = list(x = "topleft", bty = "n"))
}

# ------------------------------------------------------------------------------
# 5. Display everything for the whole portfolio
# ------------------------------------------------------------------------------
# save_plots = TRUE also writes every chart as a PNG file into 'plots/'.
display_portfolio <- function(stock_list, stats, save_plots = FALSE) {
  show_plot <- function(file_name, plot_call) {
    plot_call()
    if (save_plots) {
      dir.create("plots", showWarnings = FALSE)
      png(file.path("plots", file_name), width = 1000, height = 600)
      plot_call()
      dev.off()
    }
  }

  for (sym in names(stock_list)) {
    display_stock_data(stock_list[[sym]], sym)
    display_quote_summary(stock_list[[sym]], sym)

    show_plot(paste0(sym, "_candlestick.png"),
              function() plot_candlestick(stock_list[[sym]], sym))
    show_plot(paste0(sym, "_close_ma.png"),
              function() plot_close_with_ma(stats$details[[sym]], sym))
    show_plot(paste0(sym, "_histogram.png"),
              function() plot_close_histogram(stock_list[[sym]], sym))
  }

  display_statistics(stats$summary)
  show_plot("portfolio_comparison.png",
            function() plot_portfolio_comparison(stock_list))
  show_plot("portfolio_statistics.png",
            function() plot_statistics_bars(stats$summary))
}

# ------------------------------------------------------------------------------
# 6. Main program
# ------------------------------------------------------------------------------
stock_data <- load_stock_data("portfolio.txt")
portfolio_stats <- calculate_portfolio_statistics(stock_data)
display_portfolio(stock_data, portfolio_stats, save_plots = TRUE)
