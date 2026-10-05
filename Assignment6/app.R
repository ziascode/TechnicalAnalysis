# ==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Student: Zia Yousaf
# File:    app.R
# Purpose: Portfolio visualization dashboard built with R Shiny. The app
#          fetches stock data from the web, draws it as a line, candlestick
#          or area chart, overlays technical indicators (Moving Averages,
#          Bollinger Bands, RSI, MACD, Volume), applies trading rules and
#          annotates the bars with Buy, Sell and Hold signals.
# Builds on: Assignment 2 (portfolio.txt, quantmod data import, statistics).
# How to run: open this file in RStudio and click "Run App", or run
#          shiny::runApp() from the folder that contains this file.
# ==============================================================================


# ==============================================================================
# STEP 1: DATA COLLECTION AND SETUP
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1 Install and load the necessary packages
# ------------------------------------------------------------------------------
# shiny    : the web application framework for the dashboard
# ggplot2  : all charts
# quantmod : downloads stock data (getSymbols) and loads TTR, xts and zoo
# TTR      : technical indicators (SMA, EMA, RSI, MACD, BBands)
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "xts", "zoo")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.rstudio.com/")
  }
}
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# ------------------------------------------------------------------------------
# 1.2 Choose the data sources
# ------------------------------------------------------------------------------
# Primary source  : Yahoo Finance, for the price history of every stock.
# Second source   : FRED (Federal Reserve Economic Data), for the S&P 500
#                   index that is used as a benchmark on the comparison tab.
#                   If FRED is unavailable the app falls back to the Yahoo
#                   Finance symbol ^GSPC.
PRICE_SOURCE <- "yahoo"
BENCHMARK_FRED_SERIES <- "SP500"
BENCHMARK_YAHOO_SYMBOL <- "^GSPC"

# ------------------------------------------------------------------------------
# 1.3 Read the portfolio created in Assignment 2
# ------------------------------------------------------------------------------
# Reads 'portfolio.txt' (one stock symbol per line). The file is looked for
# in this folder first and then in the Assignment2 folder of the repository.
# If no file is found, a default portfolio is used so the app still starts.
read_portfolio <- function(candidates = c("portfolio.txt",
                                          "../Assignment2/portfolio.txt")) {
  default_symbols <- c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA")
  for (path in candidates) {
    if (file.exists(path)) {
      symbols <- toupper(trimws(readLines(path, warn = FALSE)))
      symbols <- unique(symbols[symbols != ""])
      if (length(symbols) > 0) return(symbols)
    }
  }
  default_symbols
}

portfolio_symbols <- read_portfolio()

# ------------------------------------------------------------------------------
# 1.4 Fetch historical stock data
# ------------------------------------------------------------------------------
# Downloaded data is kept in memory so that changing a widget does not
# trigger a new download for data the app already has.
data_cache <- new.env()

# Downloads daily prices for one symbol with quantmod::getSymbols() and
# returns a clean xts object with the columns Open, High, Low, Close, Volume.
#
# Exception handling:
#   - an empty or invalid symbol stops with a readable message
#   - a failed download (bad symbol, no internet) stops with a readable message
#   - rows without a closing price are removed
#   - missing Open, High or Low values are filled with the closing price
#   - missing volume is set to zero
fetch_stock_data <- function(symbol, from, to, src = PRICE_SOURCE) {
  symbol <- toupper(trimws(symbol))
  if (length(symbol) != 1 || !nzchar(symbol)) {
    stop("Please select or type a stock symbol.", call. = FALSE)
  }

  cache_key <- paste(symbol, from, to, src)
  if (exists(cache_key, envir = data_cache, inherits = FALSE)) {
    return(get(cache_key, envir = data_cache))
  }

  raw <- tryCatch(
    suppressWarnings(
      getSymbols(symbol, src = src, from = from, to = to,
                 auto.assign = FALSE, warnings = FALSE)
    ),
    error = function(e) {
      stop(sprintf("Could not download data for '%s'. Check the symbol and your internet connection. (%s)",
                   symbol, conditionMessage(e)), call. = FALSE)
    }
  )

  if (is.null(raw) || nrow(raw) == 0 || ncol(raw) < 5) {
    stop(sprintf("No price data was returned for '%s'.", symbol), call. = FALSE)
  }

  prices <- raw[, 1:5]
  colnames(prices) <- c("Open", "High", "Low", "Close", "Volume")

  # Handle missing data
  prices <- prices[!is.na(prices$Close), ]
  for (col in c("Open", "High", "Low")) {
    missing <- which(is.na(prices[, col]))
    if (length(missing) > 0) prices[missing, col] <- prices$Close[missing]
  }
  missing_volume <- which(is.na(prices$Volume))
  if (length(missing_volume) > 0) prices[missing_volume, "Volume"] <- 0

  if (nrow(prices) == 0) {
    stop(sprintf("'%s' has no usable prices in the selected period.", symbol),
         call. = FALSE)
  }

  assign(cache_key, prices, envir = data_cache)
  prices
}

# Downloads the S&P 500 benchmark. Tries FRED first and Yahoo Finance second.
# Returns a list with a data frame (Date, Close) and the name of the source,
# or NULL when both sources fail, so the app keeps working without it.
fetch_benchmark <- function(from, to) {
  fred <- tryCatch({
    cache_key <- paste("FRED", BENCHMARK_FRED_SERIES)
    if (!exists(cache_key, envir = data_cache, inherits = FALSE)) {
      series <- suppressWarnings(
        getSymbols(BENCHMARK_FRED_SERIES, src = "FRED", auto.assign = FALSE)
      )
      assign(cache_key, series, envir = data_cache)
    }
    series <- get(cache_key, envir = data_cache)
    series <- na.omit(series[paste0(from, "/", to)])
    if (nrow(series) < 2) stop("not enough data")
    data.frame(Date = as.Date(index(series)),
               Close = as.numeric(series[, 1]))
  }, error = function(e) NULL)
  if (!is.null(fred)) return(list(data = fred, source = "FRED"))

  yahoo <- tryCatch({
    series <- fetch_stock_data(BENCHMARK_YAHOO_SYMBOL, from, to)
    data.frame(Date = as.Date(index(series)),
               Close = as.numeric(series$Close))
  }, error = function(e) NULL)
  if (!is.null(yahoo)) return(list(data = yahoo, source = "Yahoo Finance"))

  NULL
}

# ------------------------------------------------------------------------------
# 1.5 Convert the data to the selected time frame
# ------------------------------------------------------------------------------
# Daily data is returned unchanged. Weekly and monthly bars are built from
# the daily data: first open, highest high, lowest low, last close and the
# total volume of each period.
convert_time_frame <- function(prices, time_frame = "Daily") {
  if (time_frame == "Daily") return(prices)
  period <- if (time_frame == "Weekly") "weeks" else "months"
  bars <- to.period(prices, period = period, OHLC = TRUE)
  colnames(bars) <- c("Open", "High", "Low", "Close", "Volume")
  bars
}

# Number of extra calendar days downloaded before the selected start date.
# The indicators need this history to "warm up", so that for example a 50 bar
# moving average already has a value on the first bar shown on the chart.
WARMUP_DAYS <- c(Daily = 450, Weekly = 1600, Monthly = 2600)


# ==============================================================================
# STEP 3 (functions): TECHNICAL INDICATORS
# The functions are defined here, before the app, and are used by the server.
# ==============================================================================

# Moving average that returns NA instead of an error when there are fewer
# bars than the window.
moving_average <- function(x, n, type = "SMA") {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  as.numeric(if (type == "EMA") EMA(x, n = n) else SMA(x, n = n))
}

# Takes the price bars (xts) and returns a data frame with one row per bar
# and every indicator as a column.
#
# p is a list of parameters:
#   ma_type, short_n, long_n     moving averages
#   bb_n, bb_sd                  Bollinger Bands
#   rsi_n                        Relative Strength Index
#   macd_fast, macd_slow, macd_signal   MACD
add_indicators <- function(prices, p) {
  df <- data.frame(Date = as.Date(index(prices)), coredata(prices),
                   row.names = NULL)
  close <- df$Close
  n <- nrow(df)
  na_col <- rep(NA_real_, n)

  # Moving averages (short and long)
  df$MA_Short <- moving_average(close, p$short_n, p$ma_type)
  df$MA_Long  <- moving_average(close, p$long_n, p$ma_type)

  # Bollinger Bands: moving average plus and minus a number of standard deviations
  if (n >= p$bb_n) {
    bb <- BBands(close, n = p$bb_n, sd = p$bb_sd)
    df$BB_Lower <- as.numeric(bb[, "dn"])
    df$BB_Upper <- as.numeric(bb[, "up"])
  } else {
    df$BB_Lower <- na_col
    df$BB_Upper <- na_col
  }

  # RSI: momentum oscillator between 0 and 100
  df$RSI <- if (n > p$rsi_n) as.numeric(RSI(close, n = p$rsi_n)) else na_col

  # MACD: difference of a fast and a slow EMA, and its signal line
  if (n >= p$macd_slow + p$macd_signal) {
    macd <- MACD(close, nFast = p$macd_fast, nSlow = p$macd_slow,
                 nSig = p$macd_signal, maType = "EMA", percent = FALSE)
    df$MACD        <- as.numeric(macd[, "macd"])
    df$MACD_Signal <- as.numeric(macd[, "signal"])
  } else {
    df$MACD        <- na_col
    df$MACD_Signal <- na_col
  }
  df$MACD_Hist <- df$MACD - df$MACD_Signal

  df
}


# ==============================================================================
# STEP 4 (functions): TRADING RULES
# ==============================================================================
# Four trading rules are available. Every rule labels each bar as Buy, Sell
# or Hold.
#
# 1. Moving Average Crossover
#    Buy  : the short moving average crosses above the long moving average
#    Sell : the short moving average crosses below the long moving average
#    Hold : every other bar
#
# 2. RSI Overbought / Oversold
#    Buy  : RSI crosses back above the oversold level (default 30)
#    Sell : RSI crosses back below the overbought level (default 70)
#    Hold : every other bar
#
# 3. MACD Signal Line Crossover
#    Buy  : the MACD line crosses above its signal line
#    Sell : the MACD line crosses below its signal line
#    Hold : every other bar
#
# 4. Combined (majority vote)
#    Three votes are counted on every bar:
#      trend    : short MA above (bullish) or below (bearish) the long MA
#      momentum : MACD above (bullish) or below (bearish) its signal line
#      strength : RSI above (bullish) or below (bearish) 50
#    Buy  : the majority turns bullish
#    Sell : the majority turns bearish
#    Hold : every other bar
TRADING_RULES <- c(
  "Moving Average Crossover"           = "ma",
  "RSI Overbought / Oversold"          = "rsi",
  "MACD Signal Line Crossover"         = "macd",
  "Combined (majority vote)"   = "combined"
)

# Turns a series of states (1 = bullish, -1 = bearish, 0 or NA = no
# information) into Buy / Sell / Hold signals.
#   - the last known state is carried forward over bars with no information
#   - a Buy is produced on the bar where the state turns bullish
#   - a Sell is produced on the bar where the state turns bearish
#   - signal_on_first = FALSE ignores the very first state, because no
#     crossover has actually been observed at that point
state_to_signals <- function(state, signal_on_first = FALSE) {
  state[is.na(state)] <- 0
  held <- state
  if (length(held) > 1) {
    for (i in 2:length(held)) {
      if (held[i] == 0) held[i] <- held[i - 1]
    }
  }
  previous <- c(0, head(held, -1))
  changed <- held != previous & held != 0
  if (!signal_on_first) changed <- changed & previous != 0

  signal <- rep("Hold", length(held))
  signal[changed & held == 1]  <- "Buy"
  signal[changed & held == -1] <- "Sell"
  list(state = held, signal = signal)
}

# Adds the columns Signal (Buy / Sell / Hold) and Stance (Bullish / Bearish /
# Neutral) to the indicator data frame, using the selected trading rule.
generate_signals <- function(df, rule = "ma", p) {
  ma_state   <- sign(df$MA_Short - df$MA_Long)
  macd_state <- sign(df$MACD - df$MACD_Signal)
  rsi        <- df$RSI
  rsi_prev   <- c(NA, head(rsi, -1))

  if (rule == "ma") {
    result <- state_to_signals(ma_state)

  } else if (rule == "macd") {
    result <- state_to_signals(macd_state)

  } else if (rule == "rsi") {
    events <- rep(0, nrow(df))
    valid <- !is.na(rsi) & !is.na(rsi_prev)
    events[valid & rsi_prev < p$rsi_oversold & rsi >= p$rsi_oversold] <- 1
    events[valid & rsi_prev > p$rsi_overbought & rsi <= p$rsi_overbought] <- -1
    result <- state_to_signals(events, signal_on_first = TRUE)

  } else {
    rsi_state <- sign(rsi - 50)
    votes <- cbind(ma_state, macd_state, rsi_state)
    complete <- stats::complete.cases(votes)
    score <- rowSums(votes, na.rm = TRUE)
    result <- state_to_signals(ifelse(complete, sign(score), 0))
  }

  df$Signal <- result$signal
  df$Stance <- c("Bearish", "Neutral", "Bullish")[result$state + 2]
  df
}

# One function that runs the whole pipeline for a symbol: download, convert
# to the time frame, add the indicators and generate the signals.
analyse_symbol <- function(symbol, date_from, date_to, time_frame, rule, p) {
  prices <- fetch_stock_data(symbol,
                             from = date_from - WARMUP_DAYS[[time_frame]],
                             to = date_to + 1)
  bars <- convert_time_frame(prices, time_frame)
  df <- add_indicators(bars, p)
  generate_signals(df, rule, p)
}


# ==============================================================================
# STEP 2 (functions): CHARTS
# ==============================================================================
COLOUR_UP   <- "#1a9850"
COLOUR_DOWN <- "#d73027"
COLOUR_LINE <- "#1f4e79"

chart_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top",
          legend.title = element_blank(),
          panel.grid.minor = element_blank())
}

# Half the width of a candle body, in days, for each time frame
candle_half_width <- function(time_frame) {
  switch(time_frame, Daily = 0.35, Weekly = 2.4, Monthly = 10)
}

# ------------------------------------------------------------------------------
# Price chart: line, candlestick or area, with the overlays and annotations
# ------------------------------------------------------------------------------
#   df          visible bars with indicators and signals
#   indicators  character vector of the overlays that are switched on
#   annotation  "none", "changes" (Buy and Sell only) or "all" (also Hold)
build_price_plot <- function(df, symbol, chart_type, time_frame, indicators,
                             annotation, p, rule_label, x_limits) {
  price_range <- range(c(df$Low, df$High), na.rm = TRUE)
  offset <- diff(price_range) * 0.035
  if (!is.finite(offset) || offset == 0) offset <- max(price_range) * 0.01

  g <- ggplot(df, aes(x = Date))

  # ---- Step 3: Bollinger Bands, drawn first so they sit behind the price ----
  if ("Bollinger Bands" %in% indicators && any(!is.na(df$BB_Upper))) {
    bands <- df[!is.na(df$BB_Upper), ]
    g <- g +
      geom_ribbon(data = bands, aes(ymin = BB_Lower, ymax = BB_Upper),
                  fill = "#6baed6", alpha = 0.18) +
      geom_line(data = bands, aes(y = BB_Upper), colour = "#6baed6",
                linetype = "dashed", linewidth = 0.4) +
      geom_line(data = bands, aes(y = BB_Lower), colour = "#6baed6",
                linetype = "dashed", linewidth = 0.4)
  }

  # ---- Step 2: the chart type selected by the user ----
  if (chart_type == "Candlestick") {
    w <- candle_half_width(time_frame)
    up <- df[df$Close >= df$Open, ]
    down <- df[df$Close < df$Open, ]
    g <- g +
      geom_linerange(data = up, aes(ymin = Low, ymax = High),
                     colour = COLOUR_UP) +
      geom_linerange(data = down, aes(ymin = Low, ymax = High),
                     colour = COLOUR_DOWN) +
      geom_rect(data = up, aes(xmin = Date - w, xmax = Date + w,
                               ymin = Open, ymax = Close),
                fill = COLOUR_UP, colour = COLOUR_UP) +
      geom_rect(data = down, aes(xmin = Date - w, xmax = Date + w,
                                 ymin = Close, ymax = Open),
                fill = COLOUR_DOWN, colour = COLOUR_DOWN)
  } else if (chart_type == "Area") {
    g <- g +
      geom_ribbon(aes(ymin = price_range[1] - offset, ymax = Close),
                  fill = COLOUR_LINE, alpha = 0.25) +
      geom_line(aes(y = Close), colour = COLOUR_LINE, linewidth = 0.8)
  } else {
    g <- g + geom_line(aes(y = Close), colour = COLOUR_LINE, linewidth = 0.8)
  }

  # ---- Step 3: moving averages as additional layers ----
  if ("Moving Averages" %in% indicators) {
    short_name <- sprintf("%s %d", p$ma_type, p$short_n)
    long_name  <- sprintf("%s %d", p$ma_type, p$long_n)
    ma_long <- rbind(
      data.frame(Date = df$Date, Value = df$MA_Short, Indicator = short_name),
      data.frame(Date = df$Date, Value = df$MA_Long, Indicator = long_name)
    )
    ma_long <- ma_long[!is.na(ma_long$Value), ]
    if (nrow(ma_long) > 0) {
      ma_long$Indicator <- factor(ma_long$Indicator,
                                  levels = c(short_name, long_name))
      g <- g +
        geom_line(data = ma_long, aes(y = Value, colour = Indicator),
                  linewidth = 0.8) +
        scale_colour_manual(values = stats::setNames(c("#ff7f0e", "#7b3294"),
                                                     c(short_name, long_name)))
    }
  }

  # ---- Step 4: annotate the bars with the trading signals ----
  if (annotation != "none") {
    buys  <- df[df$Signal == "Buy", ]
    sells <- df[df$Signal == "Sell", ]
    holds <- df[df$Signal == "Hold", ]

    if (annotation == "all" && nrow(holds) > 0) {
      g <- g + geom_text(data = holds, aes(y = High + offset * 0.6),
                         label = "Hold", colour = "grey45", size = 2.6,
                         check_overlap = TRUE)
    }
    if (nrow(buys) > 0) {
      g <- g +
        geom_point(data = buys, aes(y = Low - offset), shape = 24, size = 3.5,
                   fill = COLOUR_UP, colour = COLOUR_UP) +
        geom_label(data = buys, aes(y = Low - offset * 2.4), label = "BUY",
                   fill = COLOUR_UP, colour = "white", size = 3.2,
                   fontface = "bold")
    }
    if (nrow(sells) > 0) {
      g <- g +
        geom_point(data = sells, aes(y = High + offset), shape = 25, size = 3.5,
                   fill = COLOUR_DOWN, colour = COLOUR_DOWN) +
        geom_label(data = sells, aes(y = High + offset * 2.4), label = "SELL",
                   fill = COLOUR_DOWN, colour = "white", size = 3.2,
                   fontface = "bold")
    }
  }

  g +
    coord_cartesian(xlim = x_limits) +
    labs(title = sprintf("%s: %s %s chart", symbol, time_frame,
                         tolower(chart_type)),
         subtitle = if (annotation == "none") NULL else
           paste("Trading rule:", rule_label),
         x = NULL, y = "Price (USD)") +
    chart_theme()
}

# ------------------------------------------------------------------------------
# Indicator panels drawn under the price chart, on the same date axis
# ------------------------------------------------------------------------------
build_volume_plot <- function(df, time_frame, x_limits) {
  df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
  ggplot(df, aes(x = Date, y = Volume / 1e6, fill = Direction)) +
    geom_col(width = candle_half_width(time_frame) * 2, position = "identity") +
    scale_fill_manual(values = c(Up = COLOUR_UP, Down = COLOUR_DOWN)) +
    coord_cartesian(xlim = x_limits) +
    labs(x = NULL, y = "Volume (M)") +
    chart_theme() + theme(legend.position = "none")
}

build_rsi_plot <- function(df, p, x_limits) {
  ggplot(df[!is.na(df$RSI), ], aes(x = Date, y = RSI)) +
    annotate("rect", xmin = as.Date(-Inf), xmax = as.Date(Inf),
             ymin = p$rsi_oversold, ymax = p$rsi_overbought,
             fill = "#7b3294", alpha = 0.07) +
    geom_hline(yintercept = c(p$rsi_oversold, p$rsi_overbought),
               linetype = "dashed", colour = c(COLOUR_UP, COLOUR_DOWN)) +
    geom_line(colour = "#7b3294", linewidth = 0.7) +
    coord_cartesian(xlim = x_limits, ylim = c(0, 100)) +
    scale_y_continuous(breaks = c(0, p$rsi_oversold, 50, p$rsi_overbought, 100)) +
    labs(x = NULL, y = sprintf("RSI (%d)", p$rsi_n)) +
    chart_theme() + theme(legend.position = "none")
}

build_macd_plot <- function(df, p, time_frame, x_limits) {
  macd <- df[!is.na(df$MACD_Signal), ]
  macd$Direction <- ifelse(macd$MACD_Hist >= 0, "Up", "Down")
  ggplot(macd, aes(x = Date)) +
    geom_col(aes(y = MACD_Hist, fill = Direction),
             width = candle_half_width(time_frame) * 2, alpha = 0.6,
             position = "identity") +
    geom_line(aes(y = MACD), colour = COLOUR_LINE, linewidth = 0.7) +
    geom_line(aes(y = MACD_Signal), colour = "#ff7f0e", linewidth = 0.7) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_fill_manual(values = c(Up = COLOUR_UP, Down = COLOUR_DOWN)) +
    coord_cartesian(xlim = x_limits) +
    labs(x = NULL,
         y = sprintf("MACD (%d, %d, %d)", p$macd_fast, p$macd_slow,
                     p$macd_signal)) +
    chart_theme() + theme(legend.position = "none")
}

# Stacks the price chart and the indicator panels into one chart with a
# shared date axis. The panels are aligned with gtable, which is installed
# together with ggplot2. If alignment is not possible the panels are still
# drawn one under another.
draw_stacked_chart <- function(plots, heights) {
  # Only the bottom panel keeps its date labels
  last <- length(plots)
  for (i in seq_along(plots)) {
    if (i < last) {
      plots[[i]] <- plots[[i]] + theme(axis.text.x = element_blank())
    }
  }

  grid::grid.newpage()
  if (last == 1) {
    grid::grid.draw(ggplotGrob(plots[[1]]))
    return(invisible(NULL))
  }

  aligned <- tryCatch({
    grobs <- lapply(plots, ggplotGrob)
    combined <- Reduce(function(a, b) rbind(a, b, size = "max"), grobs)
    panel_rows <- unique(combined$layout$t[grepl("^panel",
                                                 combined$layout$name)])
    combined$heights[panel_rows] <- grid::unit(heights, "null")
    combined
  }, error = function(e) NULL)

  if (!is.null(aligned)) {
    grid::grid.draw(aligned)
  } else {
    layout <- grid::grid.layout(nrow = last, heights = grid::unit(heights, "null"))
    grid::pushViewport(grid::viewport(layout = layout))
    for (i in seq_along(plots)) {
      print(plots[[i]], vp = grid::viewport(layout.pos.row = i),
            newpage = FALSE)
    }
    grid::popViewport()
  }
  invisible(NULL)
}


# ==============================================================================
# STEP 2: SHINY APP - USER INTERFACE
# ==============================================================================
INDICATOR_CHOICES <- c("Moving Averages", "Bollinger Bands", "Volume", "RSI",
                       "MACD")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f5f7fa; }
    .app-title { background: #1f4e79; color: white; padding: 14px 20px;
                 margin: 0 -15px 18px -15px; }
    .app-title h2 { margin: 0; font-size: 24px; }
    .app-title p { margin: 2px 0 0 0; opacity: 0.85; }
    .well { background: white; border: 1px solid #dfe3e8; }
    .well h4 { color: #1f4e79; font-size: 15px; font-weight: bold;
               border-bottom: 1px solid #e5e8ec; padding-bottom: 6px;
               margin-top: 18px; }
    .well h4:first-child { margin-top: 0; }
    .tab-content { background: white; border: 1px solid #dfe3e8;
                   border-top: none; padding: 16px; }
    .cards { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 12px; }
    .card { flex: 1; min-width: 130px; border: 1px solid #dfe3e8;
            border-radius: 6px; padding: 10px 12px; background: #fafbfc; }
    .card .label { display: block; padding: 0; font-size: 12px;
                   color: #6b7280; font-weight: normal; text-align: left; }
    .card .value { font-size: 20px; font-weight: bold; }
    .buy { color: #1a9850; } .sell { color: #d73027; } .hold { color: #6b7280; }
  "))),

  div(class = "app-title",
      h2("Portfolio Technical Analysis Dashboard"),
      p("BDA400 Assignment 6, Zia Yousaf")),

  sidebarLayout(
    # --------------------------------------------------------------------------
    # Interactive widgets
    # --------------------------------------------------------------------------
    sidebarPanel(
      width = 3,

      h4("Data"),
      selectizeInput("symbol", "Stock symbol:",
                     choices = portfolio_symbols,
                     selected = portfolio_symbols[1],
                     options = list(create = TRUE,
                                    placeholder = "Select or type a symbol")),
      helpText("Symbols come from portfolio.txt. Type any other Yahoo Finance",
               "symbol and press Enter to add it."),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date(),
                     max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),

      h4("Chart"),
      radioButtons("chart_type", "Chart type:",
                   choices = c("Candlestick", "Line", "Area"), inline = TRUE),

      h4("Technical indicators"),
      checkboxGroupInput("technical_indicators",
                         "Turn each overlay on or off:",
                         choices = INDICATOR_CHOICES,
                         selected = c("Moving Averages", "Volume")),
      conditionalPanel(
        "input.technical_indicators.indexOf('Moving Averages') > -1 || input.trading_rule == 'ma' || input.trading_rule == 'combined'",
        radioButtons("ma_type", "Moving average type:",
                     choices = c("SMA", "EMA"), inline = TRUE),
        fluidRow(
          column(6, numericInput("short_n", "Short MA:", value = 20,
                                 min = 2, max = 100)),
          column(6, numericInput("long_n", "Long MA:", value = 50,
                                 min = 5, max = 200))
        )
      ),
      conditionalPanel(
        "input.technical_indicators.indexOf('Bollinger Bands') > -1",
        fluidRow(
          column(6, numericInput("bb_n", "Bands period:", value = 20,
                                 min = 5, max = 100)),
          column(6, numericInput("bb_sd", "Std. deviations:", value = 2,
                                 min = 0.5, max = 4, step = 0.5))
        )
      ),
      conditionalPanel(
        "input.technical_indicators.indexOf('RSI') > -1 || input.trading_rule == 'rsi' || input.trading_rule == 'combined'",
        numericInput("rsi_n", "RSI period:", value = 14, min = 2, max = 50),
        sliderInput("rsi_levels", "RSI oversold and overbought levels:",
                    min = 5, max = 95, value = c(30, 70), step = 5)
      ),
      conditionalPanel(
        "input.technical_indicators.indexOf('MACD') > -1 || input.trading_rule == 'macd' || input.trading_rule == 'combined'",
        fluidRow(
          column(4, numericInput("macd_fast", "MACD fast",, value = 12,
                                 min = 2, max = 50)),
          column(4, numericInput("macd_slow", "MACD slow",, value = 26,
                                 min = 5, max = 100)),
          column(4, numericInput("macd_signal", "MACD signal",, value = 9,
                                 min = 2, max = 50))
        )
      ),

      h4("Trading rules"),
      selectInput("trading_rule", "Trading rule:", choices = TRADING_RULES),
      radioButtons("annotation", "Annotate the bars with:",
                   choices = c("Buy and Sell signals" = "changes",
                               "Buy, Sell and Hold on every bar" = "all",
                               "No annotations" = "none")),
      downloadButton("download_data", "Download data (CSV)")
    ),

    # --------------------------------------------------------------------------
    # Output area
    # --------------------------------------------------------------------------
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("Chart",
                 uiOutput("summary_cards"),
                 plotOutput("stock_chart", height = "auto")),
        tabPanel("Signals",
                 h4("Buy and Sell signals in the selected date range"),
                 textOutput("signal_summary"),
                 br(),
                 tableOutput("signal_table")),
        tabPanel("Portfolio Overview",
                 h4("Latest signal for every stock in portfolio.txt"),
                 p("Uses the time frame, indicator settings and trading rule",
                   "selected in the sidebar."),
                 tableOutput("portfolio_table")),
        tabPanel("Performance Comparison",
                 plotOutput("comparison_chart", height = "480px"),
                 textOutput("benchmark_note")),
        tabPanel("Data",
                 h4("Price bars with indicators and signals (latest first)"),
                 tableOutput("data_table")),
        tabPanel("Trading Rules",
                 uiOutput("rules_help"))
      )
    )
  )
)


# ==============================================================================
# STEP 2, 3 AND 4: SHINY APP - SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ----------------------------------------------------------------------------
  # Validated parameters from the widgets
  # ----------------------------------------------------------------------------
  params <- reactive({
    numbers <- list(input$short_n, input$long_n, input$bb_n, input$bb_sd,
                    input$rsi_n, input$macd_fast, input$macd_slow,
                    input$macd_signal)
    validate(
      need(all(vapply(numbers, function(v) isTruthy(v) && is.numeric(v) && v > 0,
                      logical(1))),
           "Please enter a positive number in every indicator setting.")
    )
    validate(
      need(input$short_n >= 2 && input$long_n <= 200,
           "Moving average periods must be between 2 and 200."),
      need(input$short_n < input$long_n,
           "The short moving average must be shorter than the long moving average."),
      need(input$macd_fast < input$macd_slow,
           "The MACD fast period must be shorter than the slow period.")
    )
    list(
      ma_type = input$ma_type,
      short_n = as.integer(input$short_n),
      long_n = as.integer(input$long_n),
      bb_n = as.integer(input$bb_n),
      bb_sd = input$bb_sd,
      rsi_n = as.integer(input$rsi_n),
      rsi_oversold = input$rsi_levels[1],
      rsi_overbought = input$rsi_levels[2],
      macd_fast = as.integer(input$macd_fast),
      macd_slow = as.integer(input$macd_slow),
      macd_signal = as.integer(input$macd_signal)
    )
  })

  date_range <- reactive({
    dates <- input$date_range
    validate(
      need(length(dates) == 2 && !any(is.na(dates)),
           "Please select a start date and an end date."),
      need(dates[1] < dates[2], "The start date must be before the end date.")
    )
    as.Date(dates)
  })

  rule_label <- reactive({
    names(TRADING_RULES)[TRADING_RULES == input$trading_rule]
  })

  # ----------------------------------------------------------------------------
  # Step 1 and Step 4: fetch the data, add the indicators, generate the signals
  # ----------------------------------------------------------------------------
  analysis <- reactive({
    validate(need(isTruthy(input$symbol), "Please select or type a stock symbol."))
    dates <- date_range()
    p <- params()

    full <- tryCatch(
      withProgress(message = paste("Loading", toupper(input$symbol), "..."), {
        analyse_symbol(input$symbol, dates[1], dates[2], input$time_frame,
                       input$trading_rule, p)
      }),
      error = function(e) e
    )
    if (inherits(full, "error")) {
      validate(need(FALSE, conditionMessage(full)))
    }

    # Filter the data on the selected date range
    visible <- full[full$Date >= dates[1] & full$Date <= dates[2], ]
    validate(need(nrow(visible) >= 2,
                  "Not enough data in this date range. Select a longer range or a shorter time frame."))
    visible
  })

  # ----------------------------------------------------------------------------
  # Summary cards above the chart
  # ----------------------------------------------------------------------------
  output$summary_cards <- renderUI({
    df <- analysis()
    last <- df[nrow(df), ]
    previous <- df[nrow(df) - 1, ]
    change <- last$Close - previous$Close
    change_pct <- change / previous$Close * 100
    change_class <- if (change >= 0) "buy" else "sell"
    signal_class <- tolower(last$Signal)

    card <- function(label, value, class = "") {
      div(class = "card", span(class = "label", label),
          span(class = paste("value", class), value))
    }
    div(class = "cards",
        card("Symbol", toupper(input$symbol)),
        card(paste("Last close,", format(last$Date)),
             sprintf("%.2f", last$Close)),
        card("Change", sprintf("%+.2f (%+.2f%%)", change, change_pct),
             change_class),
        card("RSI", if (is.na(last$RSI)) "n/a" else sprintf("%.1f", last$RSI)),
        card("Trend", last$Stance,
             switch(last$Stance, Bullish = "buy", Bearish = "sell", "hold")),
        card("Signal on last bar", last$Signal, signal_class))
  })

  # ----------------------------------------------------------------------------
  # Step 2, 3 and 4: the main chart
  # ----------------------------------------------------------------------------
  # Each indicator in the checkbox group switches its own overlay on or off.
  # Moving averages and Bollinger Bands are layers on the price chart. Volume,
  # RSI and MACD have their own scale, so they are stacked under the price
  # chart on the same date axis.
  sub_panels <- reactive({
    intersect(c("Volume", "RSI", "MACD"), input$technical_indicators)
  })

  output$stock_chart <- renderPlot({
    df <- analysis()
    p <- params()
    indicators <- input$technical_indicators
    w <- candle_half_width(input$time_frame)
    x_limits <- c(min(df$Date) - w * 2, max(df$Date) + w * 2)

    plots <- list(build_price_plot(df, toupper(input$symbol), input$chart_type,
                                   input$time_frame, indicators,
                                   input$annotation, p, rule_label(), x_limits))
    heights <- 3.2

    if ("Volume" %in% indicators) {
      plots[[length(plots) + 1]] <- build_volume_plot(df, input$time_frame,
                                                      x_limits)
      heights <- c(heights, 0.9)
    }
    if ("RSI" %in% indicators) {
      validate(need(any(!is.na(df$RSI)),
                    "Not enough data to calculate RSI for this period."))
      plots[[length(plots) + 1]] <- build_rsi_plot(df, p, x_limits)
      heights <- c(heights, 1.1)
    }
    if ("MACD" %in% indicators) {
      validate(need(any(!is.na(df$MACD_Signal)),
                    "Not enough data to calculate MACD for this period."))
      plots[[length(plots) + 1]] <- build_macd_plot(df, p, input$time_frame,
                                                    x_limits)
      heights <- c(heights, 1.1)
    }

    draw_stacked_chart(plots, heights)
  }, height = function() 460 + 150 * length(sub_panels()))

  # ----------------------------------------------------------------------------
  # Step 4: table of the signals
  # ----------------------------------------------------------------------------
  output$signal_summary <- renderText({
    df <- analysis()
    sprintf("%s, %s bars, rule: %s. Buy signals: %d. Sell signals: %d. Hold bars: %d.",
            toupper(input$symbol), tolower(input$time_frame), rule_label(),
            sum(df$Signal == "Buy"), sum(df$Signal == "Sell"),
            sum(df$Signal == "Hold"))
  })

  output$signal_table <- renderTable({
    df <- analysis()
    signals <- df[df$Signal != "Hold",
                  c("Date", "Signal", "Close", "MA_Short", "MA_Long", "RSI",
                    "MACD", "MACD_Signal")]
    validate(need(nrow(signals) > 0,
                  "The selected trading rule produced no Buy or Sell signals in this date range."))
    signals <- signals[order(signals$Date, decreasing = TRUE), ]
    signals$Date <- format(signals$Date)
    names(signals) <- c("Date", "Signal", "Close", "Short MA", "Long MA", "RSI",
                        "MACD", "MACD Signal")
    signals
  }, striped = TRUE, hover = TRUE, digits = 2, na = "")

  # ----------------------------------------------------------------------------
  # Portfolio overview: the same analysis for every symbol in portfolio.txt
  # ----------------------------------------------------------------------------
  output$portfolio_table <- renderTable({
    dates <- date_range()
    p <- params()
    rule <- input$trading_rule
    time_frame <- input$time_frame

    rows <- withProgress(message = "Loading portfolio ...", {
      lapply(portfolio_symbols, function(sym) {
        incProgress(1 / length(portfolio_symbols), detail = sym)
        # One failed symbol must not stop the other symbols
        tryCatch({
          df <- analyse_symbol(sym, dates[1], dates[2], time_frame, rule, p)
          df <- df[df$Date >= dates[1] & df$Date <= dates[2], ]
          if (nrow(df) < 2) stop("not enough data")
          last <- df[nrow(df), ]
          events <- df[df$Signal != "Hold", ]
          data.frame(
            Symbol = sym,
            `Last Close` = sprintf("%.2f", last$Close),
            `Period Change` = sprintf("%+.1f%%",
                                      (last$Close / df$Close[1] - 1) * 100),
            RSI = if (is.na(last$RSI)) "" else sprintf("%.1f", last$RSI),
            Trend = last$Stance,
            `Last Signal` = if (nrow(events) > 0) sprintf(
              "%s on %s", events$Signal[nrow(events)],
              format(events$Date[nrow(events)])) else "None in range",
            `Action Today` = last$Signal,
            check.names = FALSE, stringsAsFactors = FALSE)
        }, error = function(e) {
          data.frame(Symbol = sym, `Last Close` = "", `Period Change` = "",
                     RSI = "", Trend = "",
                     `Last Signal` = "Data not available",
                     `Action Today` = "", check.names = FALSE,
                     stringsAsFactors = FALSE)
        })
      })
    })
    do.call(rbind, rows)
  }, striped = TRUE, hover = TRUE)

  # ----------------------------------------------------------------------------
  # Performance comparison: every stock rebased to 100, plus the S&P 500
  # ----------------------------------------------------------------------------
  benchmark <- reactive({
    dates <- date_range()
    fetch_benchmark(dates[1], dates[2])
  })

  output$comparison_chart <- renderPlot({
    dates <- date_range()
    series <- withProgress(message = "Loading comparison ...", {
      lapply(portfolio_symbols, function(sym) {
        tryCatch({
          prices <- fetch_stock_data(sym, from = dates[1], to = dates[2] + 1)
          data.frame(Date = as.Date(index(prices)),
                     Value = as.numeric(prices$Close) /
                       as.numeric(prices$Close[1]) * 100,
                     Series = sym)
        }, error = function(e) NULL)
      })
    })
    series <- do.call(rbind, series)
    validate(need(!is.null(series) && nrow(series) > 0,
                  "No data could be downloaded for the portfolio."))

    g <- ggplot(series, aes(x = Date, y = Value, colour = Series)) +
      geom_line(linewidth = 0.8)

    bench <- benchmark()
    if (!is.null(bench)) {
      bench_df <- bench$data
      bench_df$Value <- bench_df$Close / bench_df$Close[1] * 100
      g <- g + geom_line(data = bench_df, aes(x = Date, y = Value),
                         inherit.aes = FALSE, colour = "black",
                         linetype = "dashed", linewidth = 0.9)
    }

    g + geom_hline(yintercept = 100, colour = "grey50") +
      labs(title = "Portfolio performance, rebased to 100 at the start date",
           subtitle = if (!is.null(bench)) "Dashed black line: S&P 500 index" else NULL,
           x = NULL, y = "Rebased closing price") +
      chart_theme()
  })

  output$benchmark_note <- renderText({
    bench <- benchmark()
    if (is.null(bench)) {
      "The S&P 500 benchmark could not be downloaded from FRED or Yahoo Finance, so only the portfolio stocks are shown."
    } else {
      paste0("Stock prices: Yahoo Finance. S&P 500 benchmark: ", bench$source, ".")
    }
  })

  # ----------------------------------------------------------------------------
  # Data table and download
  # ----------------------------------------------------------------------------
  output$data_table <- renderTable({
    df <- analysis()
    df <- df[order(df$Date, decreasing = TRUE), ]
    df <- head(df, 60)
    df$Date <- format(df$Date)
    df$Volume <- format(df$Volume, big.mark = ",", scientific = FALSE)
    df[, c("Date", "Open", "High", "Low", "Close", "Volume", "MA_Short",
           "MA_Long", "RSI", "MACD", "MACD_Signal", "Stance", "Signal")]
  }, striped = TRUE, hover = TRUE, digits = 2, na = "")

  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("%s_%s_signals.csv", toupper(input$symbol),
              tolower(input$time_frame))
    },
    content = function(file) {
      write.csv(analysis(), file, row.names = FALSE)
    }
  )

  # ----------------------------------------------------------------------------
  # Explanation of the trading rules, with the current parameter values
  # ----------------------------------------------------------------------------
  output$rules_help <- renderUI({
    p <- params()
    tagList(
      h4("1. Moving Average Crossover"),
      tags$ul(
        tags$li(sprintf("Buy: the %s %d crosses above the %s %d.",
                        p$ma_type, p$short_n, p$ma_type, p$long_n)),
        tags$li(sprintf("Sell: the %s %d crosses below the %s %d.",
                        p$ma_type, p$short_n, p$ma_type, p$long_n)),
        tags$li("Hold: every other bar.")),
      h4("2. RSI Overbought / Oversold"),
      tags$ul(
        tags$li(sprintf("Buy: RSI (%d) crosses back above the oversold level of %g.",
                        p$rsi_n, p$rsi_oversold)),
        tags$li(sprintf("Sell: RSI (%d) crosses back below the overbought level of %g.",
                        p$rsi_n, p$rsi_overbought)),
        tags$li("Hold: every other bar.")),
      h4("3. MACD Signal Line Crossover"),
      tags$ul(
        tags$li(sprintf("Buy: MACD (%d, %d) crosses above its %d period signal line.",
                        p$macd_fast, p$macd_slow, p$macd_signal)),
        tags$li("Sell: MACD crosses below its signal line."),
        tags$li("Hold: every other bar.")),
      h4("4. Combined (majority vote)"),
      tags$ul(
        tags$li("Three votes per bar: short MA against long MA, MACD against its signal line, and RSI against 50."),
        tags$li("Buy: at least two of the three turn bullish."),
        tags$li("Sell: at least two of the three turn bearish."),
        tags$li("Hold: every other bar.")),
      p("All periods and levels can be changed in the sidebar. The signals are",
        "produced by simple rules for learning purposes and are not financial advice.")
    )
  })
}

# ==============================================================================
# RUN THE APP
# ==============================================================================
shinyApp(ui, server)
