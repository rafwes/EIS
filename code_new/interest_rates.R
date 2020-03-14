## This code imports raw interest rates and stock price data.
## For each t-bill interest rate datapoint a equivalent 4-week stock return will be calculated.
## Interest rate will be inflation deflated by region.

library(dplyr)
library(lubridate)

#getwd()
#setwd()

print("Let's Start")
step=as.integer(1)
### ======================================== ###
###   Raw Data Import
### ======================================== ###


## Imports Weekly T-Bill Rates from FRED
## Description: 4-Week Treasury Bill: Secondary Market Rate (WTB4WK)
## Frequency: Weekly, Ending Friday -- Averages of business days.
## Unit: Percent, Not Seasonally Adjusted 
## Dates: January 2003 - December 2016
## Source: https://fred.stlouisfed.org/series/WTB4WK
## Primary Source: Board of Governors of the Federal Reserve System (US)
## Retrival Date: March 12, 2020

raw_tbill_weekly = read.csv('data_raw/fred_tbill_rates_weekly.csv',
                          colClasses=c("Date","numeric"))


## Imports Monthly T-Bill Rates from FRED
## Description: 4-Week Treasury Bill: Secondary Market Rate (TB4WK)
## Frequency: Monthly -- Averages of business days.
## Unit: Percent, Not Seasonally Adjusted 
## Dates: January 2003 - December 2016
## Source: https://fred.stlouisfed.org/series/TB4WK
## Primary Source: Board of Governors of the Federal Reserve System (US)
## Retrival Date: March 12, 2020

raw_tbill_monthly = read.csv('data_raw/fred_tbill_rates_monthly.csv',
                           colClasses=c("Date","numeric"))


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

# Renaming tbill dataframes since no sanitization is needed.

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

cpi_monthly <- cpi_full %>% filter(!((PERIOD == "M13")|(PERIOD == "S01")|(PERIOD == "S02")))
rm(cpi_full)

## Adds a proper date column for CPI data and drops strange year/period columns

cpi_monthly <- cbind(tbill_monthly$DATE, cpi_monthly)
cpi_monthly <- rename(cpi_monthly, "DATE" = "tbill_monthly$DATE")
cpi_monthly$YEAR <- NULL
cpi_monthly$PERIOD <- NULL

## Drop all stock data columns except "Date" and "Adjusted Close"

stocks_daily <- raw_stock_prices %>% select(Date,Adj.Close)
rm(raw_stock_prices)
colnames(stocks_daily) <- c("DATE","CLOSE")

sprintf("Step %i: Finished Data Sanitation", step)
step <- step+1

### ======================================== ###
###   Interest Rates Calculation
### ======================================== ###


## Creates a daily stock index using "adjusted closing" prices.
#stocks_daily$STOCK_INDEX <- 100*(stocks_daily$CLOSE / stocks_daily$CLOSE[1])


############# garbage bin
#week(raw_tbill_monthly$DATE)
