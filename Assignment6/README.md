# TechnicalAnalysis

Repository for BDA400 (Data Science Tools and Techniques) assignments.
Student: Zia Yousaf

## Structure

| Folder | Contents |
|---|---|
| Assignment2 | Technical Analysis using R, Preliminary Stage |
| Assignment6 | Technical Analysis using R, Visualization Phase (R Shiny dashboard) |

## Assignment2

| File | Description |
|---|---|
| portfolio.txt | Stock symbols in the portfolio, one per line |
| stock_functions.R | Functions to load stock data with quantmod and compute statistics |
| display_output.R | Functions to display data, statistics and charts. Runs the full analysis |
| ZiaYousaf_BDA400_A02.docx | Assignment report with screenshots |

How to run: open RStudio, set the working directory to the Assignment2 folder, open display_output.R and click Source.

## Assignment6

| File | Description |
|---|---|
| app.R | R Shiny portfolio dashboard: charts, technical indicators, trading rules and signal annotations |
| portfolio.txt | Stock symbols in the portfolio, one per line (same list as Assignment 2) |
| ZiaYousaf_BDA400_A06.docx | Cover page with the project description and repository link |

How to run: open Assignment6/app.R in RStudio and click Run App.

The code in app.R is organized in sections that follow the assignment steps:

1. Data Collection and Setup: packages, data sources, portfolio file, data download with error handling
2. Visualizing Stock Data: Shiny user interface, widgets, line, candlestick and area charts
3. Overlay Technical Indicators: Moving Averages, Bollinger Bands, Volume, RSI and MACD with on and off toggles
4. Trading Rules and Annotations: four trading rules with adjustable parameters and Buy, Sell and Hold annotations
