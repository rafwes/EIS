## This code imports daily T-Bill rates and regional CPI data
## to create regionally deflated indexes for t-bills.

# rm(list=ls())
# 
# library(dplyr)
# library(tidyr)
# library(tibble)
# library(readr)
# library(stringr)
# library(zoo)
# library(reshape2)
# library(ISOweek)
# library(lubridate)
# library(prophet)
# library(plm)
# library(conflicted)
# 
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
# conflict_prefer("lead", "dplyr")
# conflict_prefer("as.Date", "base")
# conflict_prefer("as.Date.numeric", "base")
# conflict_prefer("between", "dplyr")
# 
# base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
# base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

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
## Retrieval Date: March 17, 2020

raw_tbill_daily <-
  read.csv(file.path(base_path,"EIS/rawdata/fred_tbill_rates_daily.csv"),
           col.names = c("DATE","RATE_360"),
           colClasses = c("Date","numeric"),
           na.strings = c("NA","."))

## Obs: The Bank Discount rate is the rate at which a Bill is quoted 
## in the secondary market and is based on the par value, 
## amount of the discount, and a 360-day year.


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
## Retrieval Date: March 12, 2020

## Sets column names and types for import
c_names <- c("SERIES",
             "YEAR",
             "PERIOD")

c_classes <- c("factor",
               "integer",
               "factor",
               "numeric")
   
raw_cpi_northeast <- 
  read.csv(file.path(base_path,"EIS/rawdata/bls_cpi_northeast.csv"),
           col.names = c(c_names, "CPI_NE"),
           colClasses = c_classes)

raw_cpi_midwest <-
  read.csv(file.path(base_path,"EIS/rawdata/bls_cpi_midwest.csv"),
           col.names = c(c_names, "CPI_MW"), 
           colClasses = c_classes)

raw_cpi_south <- 
  read.csv(file.path(base_path,"EIS/rawdata/bls_cpi_south.csv"),
           col.names = c(c_names, "CPI_SO"),
           colClasses = c_classes)

raw_cpi_west <- 
  read.csv(file.path(base_path,"EIS/rawdata/bls_cpi_west.csv"),
           col.names = c(c_names, "CPI_WE"),
           colClasses = c_classes)

rm(c_names, c_classes)


### ======================================== ###
###   Data Sanitization
### ======================================== ###
## After this section we shall have saved all raw data into
## coherent standardized dataframes and dropped unneeded dataframes.

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
  filter( ! ((PERIOD == "M13") 
             | (PERIOD == "S01") 
             | (PERIOD == "S02")))

rm(cpi_full)

## Adds a proper date column for CPI data. Since inflation is realized over a given month, 
## assign index value the first day of next month.
cpi_monthly <- 
  add_column(cpi_monthly,
             DATE = seq(from = as.Date("2003-01-01"),
                        to = as.Date("2017-01-01"),
                        by = "month"),
             .before=1)

cpi_monthly$YEAR <- NULL
cpi_monthly$PERIOD <- NULL

## set last date one day back in order not to lose December data later
cpi_monthly <- 
  cpi_monthly %>%
  mutate(DATE = replace(DATE,
                        DATE == "2017-01-01", 
                        "2016-12-31"))

### ======================================== ###
###   T-Bill Index Calculation
### ======================================== ###
## Since each dataset for the t-bill rates are based on a 360-day return, this section
## will approximate this rate to a overnight rate and create a bond index.


## Approximates daily overnight rates in (percent)
tbill_daily <- 
  tbill_daily %>% 
  mutate(RATE_1 = RATE_360 / 360)

## Index calculations below shall follow a single base date.
base_date <- "2003-01-01"

## Sets up a new index variable and starts index with 100 on base date.
tbill_daily$INDEX_TB <- NA

tbill_daily <- 
  tbill_daily %>% 
  mutate(INDEX_TB = replace(INDEX_TB, 
                            DATE == base_date, 
                            100))

## Creates index for t-bills based on daily rates.
datapoints <- length(tbill_daily$INDEX_TB)

for(i in 1:(datapoints-1)) {
  tbill_daily$INDEX_TB[i+1] <- 
    tbill_daily$INDEX_TB[i] * (1 + tbill_daily$RATE_1[i] / 100)
  }

rm(i, datapoints)

### ======================================== ###
###   CPI Index Calculation
### ======================================== ###
## This section creates CPI indexes that match the t-bill index

## Creates a monthly CPI index per region.
cpi_monthly <- 
  cpi_monthly %>% 
  mutate(INDEX_CPI_NE = 100 * CPI_NE / CPI_NE[DATE == base_date],
         INDEX_CPI_MW = 100 * CPI_MW / CPI_MW[DATE == base_date],
         INDEX_CPI_SO = 100 * CPI_SO / CPI_SO[DATE == base_date],
         INDEX_CPI_WE = 100 * CPI_WE / CPI_WE[DATE == base_date])

### ======================================== ###
### Index Deflation
### ======================================== ###
## By the end of this section we shall have deflated t-bill
## indexes for each geographic region of interest.

## Since CPI indexes come monthly, not daily we need to 
## match monthly CPI indexes to the first of month
index_table_monthcpi <-
  tbill_daily %>% 
  select(DATE,
         INDEX_TB) %>% 
  left_join(cpi_monthly %>%
              select(DATE, 
                     INDEX_CPI_NE,
                     INDEX_CPI_MW,
                     INDEX_CPI_SO,
                     INDEX_CPI_WE),
            by = "DATE")

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
   tbill_daily)

## Deflates T-Bills, cropping last month for
## which there is no inflation data.
index_table <- 
  index_table %>% 
  mutate(INDEX_TB_DEF_NE = 100 * INDEX_TB / INDEX_CPI_NE,
         INDEX_TB_DEF_MW = 100 * INDEX_TB / INDEX_CPI_MW,
         INDEX_TB_DEF_SO = 100 * INDEX_TB / INDEX_CPI_SO,
         INDEX_TB_DEF_WE = 100 * INDEX_TB / INDEX_CPI_WE) %>%
  drop_na()

