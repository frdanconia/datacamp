# The getSymbols() function from the quantmod package provides a consistent interface to import data from various sources into your 
# workspace. By default, getSymbols() imports the data as a xts object.

# Load the quantmod package
library(quantmod)

# Import QQQ data from Yahoo! Finance
getSymbols("QQQ")

# Look at the structure of the object getSymbols created
str(QQQ)

# Look at the first few rows of QQQ
head(QQQ)

# getSymbols() imports data from Yahoo! Finance by default because src = "yahoo" by default

# Import QQQ data from Alpha Vantage
getSymbols("QQQ",src="av")

# Look at the structure of QQQ
str(QQQ)

# Import GDP data from FRED
getSymbols("GDP",src="FRED")

# Look at the structure of GDP
str(GDP)

# getSymbols() automatically created an object named like the symbol you provided.

# Assign SPY data to 'spy' using auto.assign argument
spy <- getSymbols("SPY",auto.assign = FALSE)

# Look at the structure of the 'spy' object
str(spy)

# Assign JNJ data to 'jnj' using env argument
jnj <- getSymbols("JNJ",env = NULL)

# Look at the structure of the 'jnj' object
str(jnj)

# Turning off auto.assign is useful if you want to assign the data to an object yourself.
# getSymbols() uses the Symbols and src arguments to specify the instrument and data source, respectively. The Quandl() function 
# specifies both the data source and the instrument via its code argument, in the form "DATABASE/DATASET".
# Quandl() returns a data.frame by default.
# Quandl() will not automatically assign the data to an object.
# Load the Quandl package
library(Quandl)

# Import GDP data from FRED
gdp <- Quandl("FRED/GDP")

# Look at the structure of the object returned by Quandl
str(gdp)

# The Quandl() function returns a data.frame by default. It can return other classes via the type argument.
# Import GDP data from FRED as xts
gdp_xts <- Quandl("FRED/GDP",type="xts")

# Look at the structure of gdp_xts
str(gdp_xts)

# Import GDP data from FRED as zoo
gdp_zoo <- Quandl("FRED/GDP",type="zoo")

# Look at the structure of gdp_zoo
str(gdp_zoo)

# You need the instrument identifier to import data from an internet data source. They can often be found on the data source website.
# Create an object containing the Pfizer ticker symbol
symbol <- "PFE"

# Use getSymbols to import the data
getSymbols(symbol)

# Look at the first few rows of data
head(PFE)

# Create a currency_pair object
currency_pair <- "GBP/CAD"

# Load British Pound to Canadian Dollar exchange rate data
getSymbols(currency_pair, src = "oanda")

# Examine object using str()
str(GBPCAD)

# Try to load data from 190 days ago
getSymbols(currency_pair, from = Sys.Date() - 190, to = Sys.Date(), src = "oanda")

# Create a series_name object
series_name <- "UNRATE"

# Load the data using getSymbols
getSymbols(series_name, src = "FRED")

# Create a quandl_code object
quandl_code <- "FRED/UNRATE"

# Load the data using Quandl
unemploy_rate <- Quandl(quandl_code)

# The quantmod package provides several helper functions to extract specific columns from an object, based on the column name. 
# The Op(), Hi(), Lo(), Cl(), Vo(), and Ad() functions can be used to extract the open, high, low, close, volume, and adjusted close 
# column, respectively.

# Extract the close column
dc_close <- Cl(DC)

# Look at the head of dc_close
head(dc_close)

# Extract the volume column
dc_volume <- Vo(DC)

# Look at the head of dc_volume
head(dc_volume)

# The quantmod package provides functions to extract a single column, and also has functions to extract specific sets of columns.
# Extract the high, low, and close columns
dc_hlc <- HLC(DC)

# Look at the head of dc_hlc
head(dc_hlc)

# Extract the open, high, low, close, and volume columns
dc_ohlcv <- OHLCV(DC)

# Look at the head of dc_ohlcv
head(dc_ohlcv)

# The getPrice() function in the quantmod package can extract any column by name by using the prefer argument. It can also extract 
# columns for a specific instrument by using the symbol argument, which is useful when an object contains several instruments with the 
# same price type.
# Download CME data for CL and BZ as an xts object
oil_data <- Quandl(code = c("CME/CLH2016", "CME/BZH2016"), type = "xts")

# Look at the column names of the oil_data object
colnames(oil_data)

# Extract the Open price for CLH2016
cl_open <- getPrice(oil_data, symbol = "CLH2016", prefer = "Open$")

# Look at January, 2016 using xts' ISO-8601 subsetting
cl_open["2016-01"]

# Sometimes you need to aggregate and/or transform raw data before you can continue your analysis. The Quandl() function allows you to
# specify common aggregations and transformations via the collapse and/or transform arguments. 
# Download quarterly CL and BZ prices
qtr_price <- Quandl(quandl_codes, type = "xts", collapse = "quarterly")

# View the high prices for both series
Hi(qtr_price)

# Download quarterly CL and BZ returns
qtr_return <- Quandl(quandl_codes, type = "xts", collapse = "quarterly", transform = "rdiff")

# View the settle price returns for both series
getPrice(qtr_return, prefer = "Settle")

# Call head on each object in data_env using eapply
data_list <- eapply(data_env, head)

# Merge all the list elements into one xts object
data_merged <- do.call(merge, data_list)

# Ensure the columns are ordered: open, high, low, close
data_ohlc <- OHLC(data_merged)

# Symbols
symbols <- c("AAPL", "MSFT", "IBM")

# Create new environment
data_env <- new.env()

# Load symbols into data_env
getSymbols(symbols,env = data_env)

# Extract the close column from each object and combine into one xts object
close_data <- do.call(merge, eapply(data_env, Cl))

# View the head of close_data0000
head(close_data)

# getSymbols() imports from Yahoo Finance by default. 
# getSymbols() imports from Yahoo Finance by default, to change the default data source with the setDefaults() function.
# Set the default to pull data from Alpha Vantage
setDefaults(getSymbols, src = "av")

# Get GOOG data
getSymbols("GOOG")

# Verify the data was actually pulled from Alpha Vantage
str(GOOG)

# Look at getSymbols.yahoo arguments
args(getSymbols.yahoo)

# Set default 'from' value for getSymbols.yahoo
setDefaults(getSymbols.yahoo, from = "2000-01-01")

# Confirm defaults were set correctly
getDefaults("getSymbols.yahoo")

# Changing the default source for one instrument is useful if multiple sources use the same symbol for different instruments. 
# For example, getSymbols("CP", src = "yahoo") would load Canadian Pacific Railway data from the New York Stock Exchange. 
# But getSymbols("CP", src = "FRED") would load Corporate Profits After Tax from the U.S. Bureau of Economic Analysis.
# setSymbolLookup() to specify the default data source for an instrument.To make getSymbols("CP") load the corporate profit data from 
# FRED instead of the railway stock data from Yahoo Finance.
# Look at the first few rows of CP
head(CP)

# Set the source for CP to FRED
setSymbolLookup(CP = list(src = "FRED"))

# Load CP data again
getSymbols("CP")

# Look at the first few rows of CP
head(CP)

# Save symbol lookup table
saveSymbolLookup("my_symbol_lookup.rda")

# Set default source for CP to "yahoo"
setSymbolLookup(CP = list(src = "yahoo"))

# Verify the default source is "yahoo"
getSymbolLookup("CP")

# Load symbol lookup table
loadSymbolLookup("my_symbol_lookup.rda")

# At some point, you might download data for an instrument that does not have a syntactically valid name. Syntactically valid names 
# contain letters, numbers, ".", and "_", and must start with a letter or a "." followed by a non-number.
# For example, the symbol for Berkshire Hathaway class A shares is "BRK-A", which is not a syntactically valid name because it contains
# a "-" character. Another example are Chinese stocks, which have symbols composed of numbers. The Yahoo Finance symbol for the SSE 
# Composite Index is "000001.SS".
# You can use the get function or backticks (`) to access objects that do not have syntactically valid names.
# Load BRK-A data
getSymbols("BRK-A")

# Use backticks and head() to look at the loaded data
head(`BRK-A`)

# Use get() to assign the BRK-A data to an object named BRK.A
BRK.A <- get("BRK-A")

# Just remember to use backticks or get() if you ever run into invalid characters.
# Create BRK.A object
BRK.A <- getSymbols("BRK-A", auto.assign = FALSE)

# Create col_names object with the column names of BRK.A
col_names <- colnames(BRK.A)

# Set BRK.A column names to syntactically valid names
colnames(BRK.A) <- make.names(col_names)

# Set name for BRK-A to BRK.A
setSymbolLookup(BRK.A = list(name = "BRK-A"))

# Set name for T (AT&T) to ATT
setSymbolLookup(ATT = list(name = "T"))

# Load BRK.A and ATT data
getSymbols(c("BRK.A", "ATT"))

# In order to create regular data from an irregular data set, the first thing you need is a regular sequence of date-times that span 
# the dates of your irregular data set. A "regular" sequence of date-times has equally-spaced time points.
# A zero-width xts object has an index of date-times, but no data columns.
# Extract the start date of the series
start_date <- start(irregular_xts)

# Extract the end date of the series
end_date <- end(irregular_xts)

# Create a regular date sequence
regular_index <- seq(start_date, end_date, by = "day")

# Create a zero-width xts object
regular_xts <- xts(, order.by = regular_index)

# Merge irregular_xts and regular_xts
merged_xts <- merge(irregular_xts, regular_xts)

# Look at the first few rows of merged_xts
head(merged_xts)

# Use the fill argument to fill NA with their previous value
merged_filled_xts <- merge(irregular_xts, regular_xts, fill = na.locf)

# Look at the first few rows of merged_filled_xts
head(merged_filled_xts)

# Sometimes two series have the same periodicy, but use different conventions to represent a timestamp. For example, monthly series 
# may be timestamped with the first or last date of the month. The different timestamp convention can cause many NA when series are 
# merged. The yearmon class from the zoo package helps solve this problem.
# Aggregate DFF to monthly
monthly_fedfunds <- apply.monthly(DFF, mean)

# Convert index to yearmon
index(monthly_fedfunds) <- as.yearmon(index(monthly_fedfunds))

# Merge FEDFUNDS with the monthly aggregate
merged_fedfunds <- merge(FEDFUNDS, monthly_fedfunds)

# Look at the first few rows of the merged object
head(merged_fedfunds)

# Fill NA forward
merged_fedfunds_locf <- na.locf(merged_fedfunds)

# Extract index values containing last day of month
aligned_last_day <- merged_fedfunds_locf[index(monthly_fedfunds)]

# Fill NA backward
merged_fedfunds_locb <- na.locf(merged_fedfunds, fromLast = TRUE)

# Extract index values containing first day of month
aligned_first_day <- merged_fedfunds_locb[index(FEDFUNDS)]

# Extract index weekdays
index_weekdays <- .indexwday(DFF)

# Find locations of Wednesdays
wednesdays <- which(index_weekdays == 3)

# Create custom end points
end_points <- c(0, wednesdays, nrow(DFF))

# Calculate weekly mean using custom end points
weekly_mean <- period.apply(DFF, end_points, mean)

# merge() uses this underlying index and returns a result with the first object's timezone.
# Create merged object with a Europe/London timezone
tz_london <- merge(london, chicago)

# Look at tz_london structure
str(tz_london)

# Create merged object with a America/Chicago timezone
tz_chicago <- merge(chicago, london)

# Look at tz_chicago structure
str(tz_chicago)

# Intra-day financial data often does not span a full 24 hour period. Most markets are usually closed at least part of the day. 
# This exercise assumes markets open at 9AM and close at 4PM Monday-Friday.
# Your data may not have an observation exactly at the market open and/or close. So, you would not be able to use start() and end() as 
# you could with the daily data. You need to specify the start and end date-times to create this regular sequence.
# The regular date-time sequence will include periods when markets are closed, but you can use xts' time-of-day subsetting to extract 
# periods the market is open.
# Create a regular date-time sequence
regular_index <- seq(as.POSIXct("2010-01-04 09:00"), as.POSIXct("2010-01-08 16:00"), by = "30 min")

# Create a zero-width xts object
regular_xts <- xts(x = NULL, order.by = regular_index)

# Merge irregular_xts and regular_xts, filling NA with their previous value
merged_xts <- merge(irregular_xts, regular_xts, fill = na.locf)

# Subset to trading day (9AM - 4PM)
trade_day <- merged_xts["T09:00/T16:00"]

# The previous exercise carried the last observation of the prior day forward into the first observation of the following day. 
# This exercise will show you how to fill missing values by trading day, without using the prior day's final value.
# Split trade_day into days
daily_list <- split(trade_day , f = "days")

# Use lapply to call na.locf for each day in daily_list
daily_filled <- lapply(daily_list, FUN = na.locf)
# do.call() and rbind() to convert daily_filled to a single xts object named filled_by_trade_day.
# Use do.call to rbind the results
filled_by_trade_day <- do.call(rbind, daily_filled)

# Intraday data can be huge, with hundreds of thousands of observations per day, millions per month, and hundreds of millions per year. 
# These data sets often need to be aggregated before you can work with them.
# You learned how to aggregate daily data in the Introduction to xts and zoo course. This exercise will use to.period() to aggregate 
# intraday data to an OHLC series. You often need to specify both period and k arguments to aggregate intraday data.
# Convert raw prices to 5-second prices
xts_5sec <- to.period(intraday_xts, period = "seconds", k = 5)

# Convert raw prices to 10-minute prices
xts_10min <- to.period(intraday_xts, period = "minutes", k = 10)

# Convert raw prices to 1-hour prices
xts_1hour <- to.period(intraday_xts, period = "hours", k = 1)

# getSymbols() to import a well-formatted CSV. 
# Load AMZN.csv
getSymbols("AMZN", src = "csv")

# Look at AMZN structure
str(AMZN)

# The zoo package provides several functions to import text files as zoo objects. 
# The main function is read.zoo(), which wraps read.table()

# Import AMZN.csv using read.zoo
amzn_zoo <- read.zoo("AMZN.csv", sep = ",", header = TRUE)

# Convert to xts
amzn_xts <- as.xts(amzn_zoo)

# Look at the first few rows of amzn_xts
head(amzn_xts)

# read.zoo() makes it easy to import data when the date and time are in separate columns. The index.column argument allows you to 
# specify the name or number of the column(s) containing the index data. That's all you need to do if the date and time are specified 
# in the standard format ("%Y-%m-%d" for date, and "%H:%M:%S" for time).
# Read data with read.csv
une_data <- read.csv("UNE.csv", nrows = 5)

# Look at the structure of une_data
str(une_data)

# Read data with read.zoo, specifying index columns
une_zoo <- read.zoo("UNE.csv", index.column = c("Date", "Time"), sep = ",", header = TRUE)

# Look at first few rows of data
head(une_zoo)

# Read data with read.csv
two_symbols_data <- read.csv("two_symbols.csv", nrows = 5)

# Look at the structure of two_symbols_data
str(two_symbols_data)

# Read data with read.zoo, specifying index columns
two_symbols_zoo <- read.zoo("two_symbols.csv", split = c("Symbol", "Type"), sep = ",", header = TRUE)

# Look at first few rows of data
head(two_symbols_zoo)

# na.locf() to fill missing values with the previous non-missing value. You can use interpolation when carrying the previous value 
# forward isn't appropriate. In this exercise, you will explore two interpolation methods: linear and spline.
# Linear interpolation calculates values that lie on a line between two known data points. This is a good choice for fairly linear 
# data, like a series with a strong trend. Spline interpolation is more appropriate for series without a strong trend, because it 
# calculates a non-linear approximation using multiple data points.

# fill NA using last observation carried forward
locf <- na.locf(DGS10)

# fill NA using linear interpolation
approx <- na.approx(DGS10)

# fill NA using spline interpolation
spline <- na.spline(DGS10)

# merge into one object
na_filled <- merge(locf, approx, spline)

# plot combined object
plot(na_filled, col = c("black", "red", "green"))

# Look at the last few rows of AAPL data
tail(AAPL)

# Plot close price
plot(Cl(AAPL))

# Plot adjusted close price
plot(Ad(AAPL))

# Look at first few rows of aapl_raw
head(aapl_raw)

# Look at first few rows of aapl_split_adjusted
head(aapl_split_adjusted)

# Plot difference between adjusted close and split-adjusted close
plot(Ad(aapl_raw) - Cl(aapl_split_adjusted))

# Plot difference between volume from the raw and split-adjusted sources
plot(Vo(aapl_raw) - Vo(aapl_split_adjusted))

# Stock splits can create large historical price changes even though they do not change the value of the company. So, you must adjust 
# all pre-split prices in order to calculate historical returns correctly.
# Similarly, you must adjust all pre-dividend prices. Dividends do reduce the company's value by the amount of the dividend payment, 
# but the investor's return isn't affected because they receive the offsetting dividend payment.
# In this exercise, you will learn how to use the adjustOHLC() function to adjust raw historical OHLC prices for splits and dividends, 
# so historical returns can be calculated accurately.
# Yahoo Finance provides raw prices and a split- and dividend-adjusted close column. The output of adjustOHLC() should match Yahoo's 
# adjusted close column. AAPL data from Yahoo Finance is already loaded in your workspace.

# Look at first few rows of AAPL
head(AAPL)

# Adjust AAPL for splits and dividends
aapl_adjusted <- adjustOHLC(AAPL)

# Look at first few rows of aapl_adjusted
head(aapl_adjusted)

# Download AAPL split data
splits <- getSplits("AAPL")

# Download AAPL dividend data
dividends <- getDividends("AAPL")

# Look at the first few rows of dividends
head(dividends)

# Download unadjusted AAPL dividend data
raw_dividends <- getDividends("AAPL", split.adjust = FALSE)

# Look at the first few rows of raw_dividends
head(raw_dividends)

# If you only have close prices, you can adjust them with adjRatios(). It has 3 arguments: splits, dividends, and close. It returns an 
# xts object with split and dividend adjustment ratios in columns "Split" and "Div", respectively.
# You need to provide split data via the splits argument to calculate the split ratio. To calculate the dividend ratio, you need to
# provide raw dividends and raw prices via the dividends and close arguments, respectively.
# Once you have the split and dividend adjustment ratios, you calculate the adjusted price multiplying the unadjusted price by both 
# the split and dividend adjustment ratios.
# Calculate split and dividend adjustment ratios
ratios <- adjRatios(splits = splits, dividends = raw_dividends, close = Cl(AAPL))

# Calculate adjusted close for AAPL
aapl_adjusted <- Cl(AAPL) * ratios[, "Split"] * ratios[, "Div"]

# Look at first few rows of Yahoo adjusted close
head(Ad(AAPL))

# Look at first few rows of aapl_adjusted
head(aapl_adjusted)
