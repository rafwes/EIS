## This code imports raw interest rates and stock price data.
## For each t-bill interest rate datapoint a equivalent 4-week stock return will be calculated.
## Interest rate will be inflation deflated by region.

rm(list=ls())

library(tidyverse)
#library(dplyr)
#library(tidyr)
#library(lubridate)

#getwd()
#setwd()

print("Let's Start")
step=as.integer(1)

### ======================================== ###
###   Raw Data Import
### ======================================== ###


## Imports Daily T-Bill Rates from FRED
## Description: 4-Week Treasury Bill: Secondary Market Rate (DTB4WK)
## Frequency: Daily -- Discount Basis*
## Unit: Percent, Not Seasonally Adjusted 
## Yields in percent per annum
## Dates: January 2003 - December 2016
## Source: https://fred.stlouisfed.org/series/DTB4WK
## Primary Source: Board of Governors of the Federal Reserve System (US)
## Retrival Date: March 17, 2020

raw_tbill_daily = read.csv('data_raw/fred_tbill_rates_daily.csv',
                           col.names=c("DATE","RATE_360"),
                           colClasses=c("Date","numeric"),
                           na.strings=c("NA","."))

## Imports Weekly T-Bill Rates from FRED
## Description: 4-Week Treasury Bill: Secondary Market Rate (WTB4WK)
## Frequency: Weekly, Ending Friday -- Averages of business days -- Discount Basis*
## Unit: Percent, Not Seasonally Adjusted 
## Yields in percent per annum
## Dates: January 2003 - December 2016
## Source: https://fred.stlouisfed.org/series/WTB4WK
## Primary Source: Board of Governors of the Federal Reserve System (US)
## Retrival Date: March 12, 2020

raw_tbill_weekly = read.csv('data_raw/fred_tbill_rates_weekly.csv',
                          colClasses=c("Date","numeric"))


## Imports Monthly T-Bill Rates from FRED
## Description: 4-Week Treasury Bill: Secondary Market Rate (TB4WK)
## Frequency: Monthly -- Averages of business days -- Discount Basis*
## Unit: Percent, Not Seasonally Adjusted
## Yields in percent per annum
## Dates: January 2003 - December 2016
## Source: https://fred.stlouisfed.org/series/TB4WK
## Primary Source: Board of Governors of the Federal Reserve System (US)
## Retrival Date: March 12, 2020

raw_tbill_monthly = read.csv('data_raw/fred_tbill_rates_monthly.csv',
                           colClasses=c("Date","numeric"))

## Obs: The Bank Discount rate is the rate at which a Bill is quoted in the secondary market 
## and is based on the par value, amount of the discount and a 360-day year.


## Imports Daily S&P500 Stock Prices
## Description: S&P 500 (^GSPC)
## Frequency: Daily
## Unit: USD
## Dates: January 2003 - December 2016
## Source: https://finance.yahoo.com/quote/^GSPC/history
## Primary Source: ICE Data Services
## Retrival Date: March 12, 2020

raw_stock_prices = read.csv('data_raw/yahoo_sp500_stockprices_daily.csv', 
                            colClasses=c("Date",rep("numeric",6)))


## Imports CPI Monthly Data
## Description: CPI for All Urban Consumers (CPI-U)
## Detail: All items in urban area, all urban consumers, not seasonally adjusted
## Frequency: Month -- Semester -- Year
## Base Period: 1982-84=100
## Dates: January 2003 - December 2016
## Series Id: CUUR0100SA0 -- Northeast
## Series Id: CUUR0300SA0 -- South
## Series Id: CUUR0200SA0 -- Midwest
## Series Id: CUUR0400SA0 -- West
## Source: U.S. Bureau of Labor Statistics
## Primary Source: U.S. Bureau of Labor Statistics
## Retrival Date: March 12, 2020

## Sets column names and types for import

c_names = c("SERIES","YEAR","PERIOD")
c_classes = c("factor","integer","factor","numeric")
   
raw_cpi_northeast = read.csv('data_raw/bls_cpi_northeast.csv', 
                             col.names=c(c_names,"CPI_NORTHEAST"), 
                             colClasses=c_classes)

raw_cpi_south = read.csv('data_raw/bls_cpi_south.csv',
                          col.names=c(c_names,"CPI_SOUTH"), 
                          colClasses=c_classes)

raw_cpi_midwest = read.csv('data_raw/bls_cpi_midwest.csv',
                           col.names=c(c_names,"CPI_MIDWEST"), 
                           colClasses=c_classes)

raw_cpi_west = read.csv('data_raw/bls_cpi_west.csv',
                        col.names=c(c_names,"CPI_WEST"), 
                        colClasses=c_classes)

rm(c_names,c_classes)


sprintf("Step %i: Finished Raw Data Import", step)
step <- step+1

### ======================================== ###
###   Data Sanitization
### ======================================== ###
## After this section we shall have saved all raw data into
## coherent standardized dataframes and dropped unneeded dataframes.


## Drops all stock data columns except "Date" and "Adjusted Close"
raw_stocks_daily_close <- raw_stock_prices %>% select(Date,Adj.Close)
rm(raw_stock_prices)
colnames(raw_stocks_daily_close) <- c("DATE","CLOSE")

## Creates missing stock prices dates (weekends and holidays) with last available data from column CLOSE
stocks_daily <- raw_stocks_daily_close %>% complete(DATE = seq.Date(min(DATE), max(DATE), by="day")) %>% fill(CLOSE)
rm(raw_stocks_daily_close)

## Creates missing daily t-bill rate with last available data from column RATE_360 
tbill_daily <- raw_tbill_daily %>% complete(DATE = seq.Date(min(DATE), max(DATE), by="day")) %>% fill(RATE_360)
rm(raw_tbill_daily)

# Renaming weekly/monthly tbill dataframes since no sanitization is needed.
tbill_monthly <- raw_tbill_monthly
tbill_weekly <- raw_tbill_weekly
rm(raw_tbill_monthly,raw_tbill_weekly)

## Drop SERIES column, we already differentiate CPI region using column names.
raw_cpi_northeast$SERIES <- NULL
raw_cpi_south$SERIES <- NULL
raw_cpi_midwest$SERIES <- NULL
raw_cpi_west$SERIES <- NULL

## Aggregates CPI regional data into a single table.
cpi_full <- left_join(raw_cpi_northeast, raw_cpi_south) %>% left_join(., raw_cpi_midwest) %>% left_join(., raw_cpi_west)
rm(raw_cpi_northeast,raw_cpi_midwest,raw_cpi_south,raw_cpi_west)

## Drops yearly and half-yearly CPI data leaving only monthly data.
cpi_monthly_a1 <- cpi_full %>% filter(!((PERIOD == "M13")|(PERIOD == "S01")|(PERIOD == "S02")))
rm(cpi_full)

## Adds a proper date column for CPI data and drops strange year/period columns
cpi_monthly <- bind_cols((tbill_monthly %>% select(DATE)), cpi_monthly_a1)
rm(cpi_monthly_a1)
cpi_monthly$YEAR <- NULL
cpi_monthly$PERIOD <- NULL


sprintf("Step %i: Finished Data Sanitation", step)
step <- step+1

### ======================================== ###
###   T-Bill Index Calculation
### ======================================== ###
## Since each dataset for the tbill rates are based on a 360-day return, this section
## will approximate this rate to a overnight rate and create a bond index.
## A similar index for stock prices will be created in later sections.

# Approximates daily overnight rates in (percent) -- See Supplementary Info for details.
tbill_daily$RATE_1 <- (tbill_daily$RATE_360)/360

# Sets up a new index column and starts index with 100 on "2003-01-01".
tbill_daily$INDEX <- NA
tbill_daily$INDEX[1] <- 100

# Creates index for t-bills based on daily rates.
datapoints <- length(tbill_daily$INDEX)
for(i in 1:(datapoints-1)) {
  tbill_daily$INDEX[i+1] <- (tbill_daily$INDEX[i] * (1+tbill_daily$RATE_1[i]/100))
}

# Creates effective 360-day and 30-day rates of return based on t-bill index
tbill_daily$RATE_EFF_360 <- (lead(tbill_daily$INDEX, n=360L) -  tbill_daily$INDEX)
tbill_daily$RATE_EFF_30 <- (lead(tbill_daily$INDEX, n=30L) -  tbill_daily$INDEX)

sprintf("Step %i: T-Bill Index Calculation", step)
step <- step+1
### ======================================== ###
###   Stock Return Calculation
### ======================================== ###
## This section creates stock returns that are equivalent to t-bill bank discount rates,
## that is, they represent a percentage return over a 360-day stock investment.


## Creates a daily stock index using "adjusted closing" prices. Base Period: "2004-01-01"

stocks_daily$STOCK_INDEX <- 100*(stocks_daily$CLOSE / stocks_daily$CLOSE[stocks_daily$DATE == "2004-01-01"])


## Calculates stock returns for a 360-days investment using index.

stocks_daily$STOCK_RETURN_360 <- stocks_daily$STOCK_INDEX - lag(stocks_daily$STOCK_INDEX, n=360L)
stock_returns_daily <- na.omit(stocks_daily %>% select(DATE,STOCK_RETURN_360))


## Analogously to weekly/monthly tbill rates, let's populate monthly/weekly stock data
## Start by taking weekly/monthly stock returns dates for which we have t-bill rates

stocks_weekly_dates <- tbill_weekly %>% select(DATE)
stocks_monthly_dates <- tbill_monthly %>% select(DATE)


# Adds corresponding stock data to their dates

stock_returns_weekly <- na.omit(left_join(stocks_weekly_dates,stock_returns_daily))
stock_returns_monthly <- na.omit(left_join(stocks_monthly_dates,stock_returns_daily))

rm(stocks_monthly_dates,stocks_weekly_dates)


sprintf("Step %i: Finished Stock Return Calculation", step)
step <- step+1

### ======================================== ###
###   
### ======================================== ###



############# garbage bin
if(FALSE) {

week(raw_tbill_monthly$DATE)
all.equal(A,stocks_weekly_dates)

class(cpi_monthly$DATE)
class(cpi_monthly$CPI_NORTHEAST)
class(stocks_monthly$DATE)
class(stocks_monthly$CLOSE)
class(stocks_monthly$STOCK_INDEX)
class(tbill_monthly$DATE)
class(tbill_monthly$TB4WK)

}

# Timeframe 2004-2014