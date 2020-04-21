## This code imports daily T-Bill rates, SP500 stock prices and
## regional CPI data to create deflated indexes for stocks and t-bills.

rm(list=ls())

library(tidyverse)
library(zoo)
library(reshape2)

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

raw_tbill_daily <-
  read.csv("data_raw/fred_tbill_rates_daily.csv",
           col.names = c("DATE","RATE_360"),
           colClasses = c("Date","numeric"),
           na.strings = c("NA","."))

## Obs: The Bank Discount rate is the rate at which a Bill is quoted 
## in the secondary market and is based on the par value, 
## amount of the discount, and a 360-day year.


## Imports Daily S&P500 Stock Prices
## Description: S&P 500 (^GSPC)
## Frequency: Daily
## Unit: USD
## Dates: January 2003 - December 2016
## Source: https://finance.yahoo.com/quote/^GSPC/history
## Primary Source: ICE Data Services
## Retrival Date: March 12, 2020

raw_stock_prices <-
  read.csv("data_raw/yahoo_sp500_stockprices_daily.csv",
           colClasses = c("Date", rep("numeric", 6)))


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
c_names <- c("SERIES",
             "YEAR",
             "PERIOD")

c_classes <- c("factor",
               "integer",
               "factor",
               "numeric")
   
raw_cpi_northeast <- 
  read.csv("data_raw/bls_cpi_northeast.csv",
           col.names = c(c_names, "CPI_NE"),
           colClasses = c_classes)

raw_cpi_midwest <-
  read.csv("data_raw/bls_cpi_midwest.csv",
           col.names = c(c_names, "CPI_MW"), 
           colClasses = c_classes)

raw_cpi_south <- 
  read.csv("data_raw/bls_cpi_south.csv",
           col.names = c(c_names, "CPI_SO"),
           colClasses = c_classes)

raw_cpi_west <- 
  read.csv("data_raw/bls_cpi_west.csv",
           col.names = c(c_names, "CPI_WE"),
           colClasses = c_classes)

rm(c_names, c_classes)


sprintf("Step %i: Finished Raw Data Import", step)
step <- step + 1

### ======================================== ###
###   Data Sanitization
### ======================================== ###
## After this section we shall have saved all raw data into
## coherent standardized dataframes and dropped unneeded dataframes.

## Drops all stock data columns except "Date" and "Adjusted Close"
raw_stocks_daily_close <-
  raw_stock_prices %>% 
  select(Date, Adj.Close)

rm(raw_stock_prices)

colnames(raw_stocks_daily_close) <- 
  c("DATE", "CLOSE")

## Creates missing stock prices dates (weekends and holidays) with last available data from column CLOSE
stocks_daily <- 
  raw_stocks_daily_close %>% 
  complete(DATE = seq.Date(min(DATE), 
                           max(DATE), 
                           by = "day")) %>% fill(CLOSE)

rm(raw_stocks_daily_close)

## Creates missing daily t-bill rate with last available data from column RATE_360 
tbill_daily <- 
  raw_tbill_daily %>% 
  complete(DATE = seq.Date(min(DATE), 
                           max(DATE), 
                           by = "day")) %>% fill(RATE_360)

rm(raw_tbill_daily)

## Drop SERIES column, we already differentiate CPI region using column names.
raw_cpi_northeast$SERIES <- NULL
raw_cpi_south$SERIES <- NULL
raw_cpi_midwest$SERIES <- NULL
raw_cpi_west$SERIES <- NULL

## Aggregates CPI regional data into a single table.
cpi_full <- 
  left_join(raw_cpi_northeast, 
            raw_cpi_south,
            by = c("YEAR", "PERIOD")) %>%
  left_join(., 
            raw_cpi_midwest,
            by = c("YEAR", "PERIOD")) %>% 
  left_join(., 
            raw_cpi_west,
            by = c("YEAR", "PERIOD"))

rm(raw_cpi_northeast,
   raw_cpi_midwest,
   raw_cpi_south,
   raw_cpi_west)

## Drops yearly and half-yearly CPI data leaving only monthly data.
cpi_monthly <-
  cpi_full %>%
  filter( ! ((PERIOD == "M13") |
               (PERIOD == "S01") |
               (PERIOD == "S02")))

rm(cpi_full)

## Adds a proper date column for CPI data and drops strange year/period/month columns
cpi_monthly <- 
  add_column(cpi_monthly,
             DATE = seq(from = as.Date("2003-01-01"),
                        to = as.Date("2016-12-31"),
                        by = "month"),
             .before=1)

cpi_monthly$YEAR <- NULL
cpi_monthly$PERIOD <- NULL


sprintf("Step %i: Finished Data Sanitization", step)
step <- step + 1

### ======================================== ###
###   T-Bill Index Calculation
### ======================================== ###
## Since each dataset for the tbill rates are based on a 360-day return, this section
## will approximate this rate to a overnight rate and create a bond index.
## A similar index for stock prices will be created in later sections.

## Approximates daily overnight rates in (percent) -- See Supplementary Info for details.
tbill_daily <- 
  tbill_daily %>% 
  mutate(RATE_1 = RATE_360 / 360)

## Index calculations below shall follow a single base date.
base_date <- "2003-01-01"

## Sets up a new index variable and starts index with 100 on base date.
tbill_daily$INDEX_TB <- NA

tbill_daily <- 
  tbill_daily %>% 
  mutate(INDEX_TB = replace(INDEX_TB, DATE == base_date, 100))

## Creates index for t-bills based on daily rates.
datapoints <- length(tbill_daily$INDEX_TB)

for(i in 1:(datapoints-1)) {
  tbill_daily$INDEX_TB[i+1] <- 
    tbill_daily$INDEX_TB[i] * (1 + tbill_daily$RATE_1[i] / 100)
}

rm(i, datapoints)

sprintf("Step %i: T-Bill Index Calculation", step)
step <- step + 1

### ======================================== ###
###   Stock and CPI Index Calculation
### ======================================== ###
## This section creates CPI and Stock indexes that match the t-bill index

## Creates a daily stock index using "adjusted closing" prices.
stocks_daily <- 
  stocks_daily %>% 
  mutate(INDEX_ST = 100 * CLOSE / CLOSE[DATE == base_date])


## Creates a monthly CPI index per region.
cpi_monthly <- 
  cpi_monthly %>% 
  mutate(INDEX_CPI_NE = 100 * CPI_NE / CPI_NE[DATE == base_date],
         INDEX_CPI_MW = 100 * CPI_MW / CPI_MW[DATE == base_date],
         INDEX_CPI_SO = 100 * CPI_SO / CPI_SO[DATE == base_date],
         INDEX_CPI_WE = 100 * CPI_WE / CPI_WE[DATE == base_date])


sprintf("Step %i: Finished Stock and CPI Index Calculation", step)
step <- step+1

### ======================================== ###
### Index Deflation
### ======================================== ###
## By the end of this section we shall have deflated indexes for both
## stocks and t-bills for each geographic region of interest


## From now on lets only work with indexes
index_table_nocpi <- 
  left_join(tbill_daily %>% 
              select(DATE, INDEX_TB), 
            stocks_daily %>% 
              select(DATE, INDEX_ST),
            by = "DATE")

## Since CPI indexes come monthly, not daily we need to 
## match monthly CPI indexes to the first of month
index_table_monthcpi <-
  left_join(index_table_nocpi,
            cpi_monthly %>% 
              select(DATE, 
                     INDEX_CPI_NE,
                     INDEX_CPI_MW,
                     INDEX_CPI_SO,
                     INDEX_CPI_WE),
            by = "DATE")

rm(index_table_nocpi)


## Interpolates CPI index linearly to create daily CPI index, 
## since we only have data for the first day of the month.
index_table <-
  index_table_monthcpi %>%
  mutate(INDEX_CPI_NE = na.approx(INDEX_CPI_NE,
                                  na.rm=FALSE),
         INDEX_CPI_MW = na.approx(INDEX_CPI_MW,
                                  na.rm=FALSE),
         INDEX_CPI_SO = na.approx(INDEX_CPI_SO,
                                  na.rm=FALSE),
         INDEX_CPI_WE = na.approx(INDEX_CPI_WE,
                                  na.rm=FALSE))

rm(index_table_monthcpi)

## Primitives not needed anymore and can be dropped
rm(cpi_monthly,
   stocks_daily,
   tbill_daily)

## Deflates T-Bills and Stock Indexes
index_table <- 
  index_table %>% 
  mutate(INDEX_TB_DEF_NE = 100 * INDEX_TB / INDEX_CPI_NE,
         INDEX_ST_DEF_NE = 100 * INDEX_ST / INDEX_CPI_NE,
         INDEX_TB_DEF_MW = 100 * INDEX_TB / INDEX_CPI_MW,
         INDEX_ST_DEF_MW = 100 * INDEX_ST / INDEX_CPI_MW,
         INDEX_TB_DEF_SO = 100 * INDEX_TB / INDEX_CPI_SO,
         INDEX_ST_DEF_SO = 100 * INDEX_ST / INDEX_CPI_SO,
         INDEX_TB_DEF_WE = 100 * INDEX_TB / INDEX_CPI_WE,
         INDEX_ST_DEF_WE = 100 * INDEX_ST / INDEX_CPI_WE)


sprintf("Step %i: Index Deflation", step)
step <- step + 1

if (FALSE) {

### ======================================== ###
### Real Return Rates
### ======================================== ###
## We are interested on stock/t-bill real returns over a specific
## period of time. Lets calculate them with the deflated indexes.

## Sets the number of days over which returns will be calculated
window_return <- 28L

## Real tbill/stock returns calculation
rates_real <- 
  index_table %>% 
  transmute(DATE,
            RATE_TB_REAL_NE = lead(INDEX_TB_DEF_NE, n = window_return) - INDEX_TB_DEF_NE,
            RATE_ST_REAL_NE = lead(INDEX_ST_DEF_NE, n = window_return) - INDEX_ST_DEF_NE,
            RATE_TB_REAL_MW = lead(INDEX_TB_DEF_MW, n = window_return) - INDEX_TB_DEF_MW,
            RATE_ST_REAL_MW = lead(INDEX_ST_DEF_MW, n = window_return) - INDEX_ST_DEF_MW,
            RATE_TB_REAL_SO = lead(INDEX_TB_DEF_SO, n = window_return) - INDEX_TB_DEF_SO,
            RATE_ST_REAL_SO = lead(INDEX_ST_DEF_SO, n = window_return) - INDEX_ST_DEF_SO,
            RATE_TB_REAL_WE = lead(INDEX_TB_DEF_WE, n = window_return) - INDEX_TB_DEF_WE,
            RATE_ST_REAL_WE = lead(INDEX_ST_DEF_WE, n = window_return) - INDEX_ST_DEF_WE)


sprintf("Step %i: Finished calculating Real Return Rates", step)
step <- step + 1


### ======================================== ###
### Smoothing Return Rates
### ======================================== ###
## Rates, specially stock returns, can be quite noisy due to market
## fluctuations and crisis in the short term. This section will 
## smooth the rates by applying moving averages.

window_smoothing <- 5L
spacing <- 7L

rates_real_smooth <- 
  rates_real %>% 
  transmute(DATE,
            RATE_TB_REAL_MW,
            RATE_TB_REAL_MW_SMOOTH = rollapply(RATE_TB_REAL_MW,
                                            window_smoothing,
                                            mean,
                                            partial = TRUE,
                                            align = "center"),
            RATE_ST_REAL_MW,
            RATE_ST_REAL_MW_SMOOTH = rollapply(RATE_ST_REAL_MW,
                                            window_smoothing,
                                            mean,
                                            partial = TRUE,
                                            align = "center"))

long_curve <- melt(rates_real_smooth %>% 
                  select(DATE,
                         RATE_ST_REAL_MW,
                         RATE_ST_REAL_MW_SMOOTH),
                  id = "DATE")


long_points <- melt(rates_real_smooth %>%
                      select(DATE, RATE_ST_REAL_MW_SMOOTH) %>%
                      filter(row_number() %% spacing == 1),
                    id = "DATE")


ggplot() + 
  geom_line(data = long_curve,
            aes(x = DATE, 
                y = value, 
                colour = variable)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(RATE_ST_REAL_MW = "gray", 
                                RATE_ST_REAL_MW_SMOOTH = "darkgreen")) + 
  geom_point(data = long_points,
             aes(x = DATE,
                 y = value,
                 color = variable))


long_curve_cut <- long_curve %>% 
                  filter(between(DATE,
                                 as.Date("2008-01-01"), 
                                 as.Date("2010-01-01")))


long_points_cut <- long_points %>% 
                   filter(between(DATE,
                                  as.Date("2008-01-01"), 
                                  as.Date("2010-01-01")))

ggplot() + 
  geom_line(data = long_curve_cut,
            aes(x = DATE, 
                y = value, 
                colour = variable)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(RATE_ST_REAL_MW = "gray", 
                                RATE_ST_REAL_MW_SMOOTH = "darkgreen")) + 
  geom_point(data = long_points_cut,
             aes(x = DATE,
                 y = value,
                 color = variable))

rm(long_curve,long_curve_cut,long_points,long_points_cut)

## Redo for TBills

window_smoothing <- 28L
spacing <- 28L

rates_real_smooth <- 
  rates_real %>% 
  transmute(DATE,
            RATE_TB_REAL_MW,
            RATE_TB_REAL_MW_SMOOTH = rollapply(RATE_TB_REAL_MW,
                                               window_smoothing,
                                               mean,
                                               partial = TRUE,
                                               align = "center"),
            RATE_ST_REAL_MW,
            RATE_ST_REAL_MW_SMOOTH = rollapply(RATE_ST_REAL_MW,
                                               window_smoothing,
                                               mean,
                                               partial = TRUE,
                                               align = "center"))

long_curve <- melt(rates_real_smooth %>% 
                     select(DATE,
                            RATE_TB_REAL_MW,
                            RATE_TB_REAL_MW_SMOOTH),
                   id = "DATE")


long_points <- melt(rates_real_smooth %>%
                      select(DATE, RATE_TB_REAL_MW_SMOOTH) %>%
                      filter(row_number() %% spacing == 1),
                    id = "DATE")


ggplot() + 
  geom_line(data = long_curve,
            aes(x = DATE, 
                y = value, 
                colour = variable)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(RATE_TB_REAL_MW = "gray", 
                                RATE_TB_REAL_MW_SMOOTH = "darkgreen")) + 
  geom_point(data = long_points,
             aes(x = DATE,
                 y = value,
                 color = variable))


long_curve_cut <- long_curve %>% 
  filter(between(DATE,
                 as.Date("2008-01-01"), 
                 as.Date("2010-01-01")))


long_points_cut <- long_points %>% 
  filter(between(DATE,
                 as.Date("2008-01-01"), 
                 as.Date("2010-01-01")))

ggplot() + 
  geom_line(data = long_curve_cut,
            aes(x = DATE, 
                y = value, 
                colour = variable)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c(RATE_TB_REAL_MW = "gray", 
                                RATE_TB_REAL_MW_SMOOTH = "darkgreen")) + 
  geom_point(data = long_points_cut,
             aes(x = DATE,
                 y = value,
                 color = variable))

rm(long_curve,long_curve_cut,long_points,long_points_cut)

}

############# garbage bin
if(FALSE) {

  ### ======================================== ###
  ### blablabla
  ### ======================================== ###
  
  
  sprintf("Step %i: blablabla", step)
  step <- step + 1
  ### ======================================== ###
  ### blablabla
  ### ======================================== ###
  
  
  
  
  
  
  
  
  
  ### Run Some Sanity checks
  
  rates_nominal_daily_1d <- 
    index_table %>% 
    transmute(DATE,
              RATE_TB_1 = lead(INDEX_TB,n=1L) - INDEX_TB,
              RATE_ST_1 = lead(INDEX_ST,n=1L) - INDEX_ST)
  
  rates_nominal_daily_28d <- 
    index_table %>% 
    transmute(DATE,
              RATE_TB_28 = lead(INDEX_TB,n=28L) - INDEX_TB,
              RATE_ST_28 = lead(INDEX_ST,n=28L) - INDEX_ST)
  
  
  rates_nominal_daily_360d <- 
    index_table %>% 
    transmute(DATE,
              RATE_TB_360 = lead(INDEX_TB,n=360L) - INDEX_TB,
              RATE_ST_360 = lead(INDEX_ST,n=360L) - INDEX_ST)
  
  rates_real_daily_360d <- 
    index_table %>% 
    transmute(DATE,
              RATE_TB_DEF_NE_360 = lead(INDEX_TB_DEF_NE,n=360L) - INDEX_TB_DEF_NE,
              RATE_TB_DEF_MW_360 = lead(INDEX_TB_DEF_MW,n=360L) - INDEX_TB_DEF_MW,
              RATE_TB_DEF_SO_360 = lead(INDEX_TB_DEF_SO,n=360L) - INDEX_TB_DEF_SO,
              RATE_TB_DEF_WE_360 = lead(INDEX_TB_DEF_WE,n=360L) - INDEX_TB_DEF_WE,
              RATE_ST_DEF_NE_360 = lead(INDEX_ST_DEF_NE,n=360L) - INDEX_ST_DEF_NE,
              RATE_ST_DEF_MW_360 = lead(INDEX_ST_DEF_MW,n=360L) - INDEX_ST_DEF_MW,
              RATE_ST_DEF_SO_360 = lead(INDEX_ST_DEF_SO,n=360L) - INDEX_ST_DEF_SO,
              RATE_ST_DEF_WE_360 = lead(INDEX_ST_DEF_WE,n=360L) - INDEX_ST_DEF_WE)
  
  rates_real_daily_1d <- 
    index_table %>% 
    transmute(DATE,
              RATE_TB_DEF_NE_1 = lead(INDEX_TB_DEF_NE,n=1L) - INDEX_TB_DEF_NE,
              RATE_ST_DEF_MW_1 = lead(INDEX_ST_DEF_NE,n=1L) - INDEX_ST_DEF_MW)
  
  
  ggplot(data=rates_nominal_daily_360d, aes(x = DATE, y = RATE_TB_360)) + 
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_real_daily_28d, aes(x = DATE, y = RATE_TB_DEF_NE_28)) + geom_line() +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_real_daily_360d, aes(x = DATE, y = RATE_TB_DEF_NE_360)) + geom_line() +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_nominal_daily_28d, aes(x = DATE, y = RATE_TB_28)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_nominal_daily_1d, aes(x = DATE, y = RATE_TB_1)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_real_daily_1d, aes(x = DATE, y = RATE_TB_DEF_NE_1)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_nominal_daily_360d, aes(x = DATE, y = RATE_TB_360)) + 
    geom_line(size = 1) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y",
                 limits = as.Date(c('2007-06-01','2008-06-01')))
  
  ggplot(data=rates_real_daily_360d, aes(x = DATE, y = RATE_ST_DEF_NE_360)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  ggplot(data=rates_real_daily_28d, aes(x = DATE, y = RATE_ST_DEF_NE_28)) +
    geom_line() +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")
  
  
  
  
  
  
week(raw_tbill_monthly$DATE)
all.equal(A,stocks_weekly_dates)
class(cpi_monthly$DATE)
class(cpi_monthly$CPI_NORTHEAST)
class(stocks_monthly$DATE)
class(stocks_monthly$CLOSE)
class(stocks_monthly$INDEX_ST)
class(tbill_monthly$DATE)
class(tbill_monthly$TB4WK)

}

# Timeframe 2004-2014