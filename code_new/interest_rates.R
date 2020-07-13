## This code imports daily T-Bill rates, SP500 stock prices and
## regional CPI data to create deflated indexes for stocks and t-bills.

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
# #library(grid)
# #library(gridExtra)
# 
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
# conflict_prefer("lead", "dplyr")
# conflict_prefer("as.Date", "base")
# conflict_prefer("as.Date.numeric", "base")
# conflict_prefer("between", "dplyr")
# 
# #base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
# base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

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
  read.csv(file.path(base_path,"EIS/data_raw/fred_tbill_rates_daily.csv"),
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
  read.csv(file.path(base_path,"EIS/data_raw/yahoo_sp500_stockprices_daily.csv"),
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
  read.csv(file.path(base_path,"EIS/data_raw/bls_cpi_northeast.csv"),
           col.names = c(c_names, "CPI_NE"),
           colClasses = c_classes)

raw_cpi_midwest <-
  read.csv(file.path(base_path,"EIS/data_raw/bls_cpi_midwest.csv"),
           col.names = c(c_names, "CPI_MW"), 
           colClasses = c_classes)

raw_cpi_south <- 
  read.csv(file.path(base_path,"EIS/data_raw/bls_cpi_south.csv"),
           col.names = c(c_names, "CPI_SO"),
           colClasses = c_classes)

raw_cpi_west <- 
  read.csv(file.path(base_path,"EIS/data_raw/bls_cpi_west.csv"),
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


######delete down####################
######delete down####################
######delete down####################

# setup data as expected by prophet
data_model_ne <- 
  cpi_monthly %>%
  transmute(ds = DATE,
            y = CPI_NE)

data_model_mw <- 
  cpi_monthly %>%
  transmute(ds = DATE,
            y = CPI_MW)

data_model_so <- 
  cpi_monthly %>%
  transmute(ds = DATE,
            y = CPI_SO)

data_model_we <- 
  cpi_monthly %>%
  transmute(ds = DATE,
            y = CPI_WE)

# fit model and decompose by issuing predict
model_ne <- prophet(seasonality.mode = 'multiplicative')
model_ne <- fit.prophet(model_ne, data_model_ne)
data_decomposed_ne <- predict(model_ne)

model_mw <- prophet(seasonality.mode = 'multiplicative')
model_mw <- fit.prophet(model_mw, data_model_mw)
data_decomposed_mw <- predict(model_mw)

model_so <- prophet(seasonality.mode = 'multiplicative')
model_so <- fit.prophet(model_so, data_model_so)
data_decomposed_so <- predict(model_so)

model_we <- prophet(seasonality.mode = 'multiplicative')
model_we <- fit.prophet(model_we, data_model_we)
data_decomposed_we <- predict(model_we)



# reconstruct de-seasonal trip data from trend and residuals

cpi_ds_ne <- 
  data_decomposed_ne %>%
  left_join(data_model_ne, by="ds") %>% 
  transmute(DATE = as.Date(ds),
            CPI_DS_NE = trend + y - yhat)

cpi_ds_mw <- 
  data_decomposed_mw %>%
  left_join(data_model_mw, by="ds") %>% 
  transmute(DATE = as.Date(ds),
            CPI_DS_MW = trend + y - yhat)

cpi_ds_so <- 
  data_decomposed_so %>%
  left_join(data_model_so, by="ds") %>% 
  transmute(DATE = as.Date(ds),
            CPI_DS_SO = trend + y - yhat)

cpi_ds_we <- 
  data_decomposed_we %>%
  left_join(data_model_we, by="ds") %>% 
  transmute(DATE = as.Date(ds),
            CPI_DS_WE = trend + y - yhat)


cpi_monthly <-
  cpi_monthly %>% 
  left_join(cpi_ds_ne, by = "DATE") %>% 
  left_join(cpi_ds_mw, by = "DATE") %>%
  left_join(cpi_ds_so, by = "DATE") %>% 
  left_join(cpi_ds_we, by = "DATE")

rm(list=ls(pattern="^model"))
rm(list=ls(pattern="^data_"))
rm(list=ls(pattern="^cpi_ds"))


if (FALSE) {

MonthlySeasonalDummiesLM <- function(x) {
  
  lm(Y ~ -1+TIME+
       M01+M02+M03+M04+
       M05+M06+M07+M08+
       M09+M10+M11+M12,
     data = x)
}


# Since CPI data is has a clear trend, a time
# variable is needed to suppress it.
CPISeasonalityMatrix <- function(x,y) {
  
  x %>%
    arrange(DATE) %>%
    transmute(Y = !!y,
              TIME = as.integer(rownames(x)),
              M01 = case_when(month(DATE) == 1 ~ 1, TRUE ~ 0),
              M02 = case_when(month(DATE) == 2 ~ 1, TRUE ~ 0),
              M03 = case_when(month(DATE) == 3 ~ 1, TRUE ~ 0),
              M04 = case_when(month(DATE) == 4 ~ 1, TRUE ~ 0),
              M05 = case_when(month(DATE) == 5 ~ 1, TRUE ~ 0),
              M06 = case_when(month(DATE) == 6 ~ 1, TRUE ~ 0),
              M07 = case_when(month(DATE) == 7 ~ 1, TRUE ~ 0),
              M08 = case_when(month(DATE) == 8 ~ 1, TRUE ~ 0),
              M09 = case_when(month(DATE) == 9 ~ 1, TRUE ~ 0),
              M10 = case_when(month(DATE) == 10 ~ 1, TRUE ~ 0),
              M11 = case_when(month(DATE) == 11 ~ 1, TRUE ~ 0),
              M12 = case_when(month(DATE) == 12 ~ 1, TRUE ~ 0))
}



model_ne <- 
  MonthlySeasonalDummiesLM(
    CPISeasonalityMatrix(
      cpi_monthly, 
      as.name("CPI_NE")))

model_mw <- 
  MonthlySeasonalDummiesLM(
    CPISeasonalityMatrix(
      cpi_monthly, 
      as.name("CPI_MW")))

model_so <- 
  MonthlySeasonalDummiesLM(
    CPISeasonalityMatrix(
      cpi_monthly, 
      as.name("CPI_SO")))

model_we <- 
  MonthlySeasonalDummiesLM(
    CPISeasonalityMatrix(
      cpi_monthly, 
      as.name("CPI_WE")))

#summary(model_we)

# reconstruct cpi index 



# "mean(coefficients(model_xx)[-1])"
# "as.numeric(coefficients(model_xx)[1])"
# Refer in the code below respectively to:
# 1. mean value of monthly dummies coefficients
# 2. trend coefficient from regression

# Reconstruct deseasonalized CPI

cpi_monthly <-
  cpi_monthly %>% 
  mutate(CPI_DS_NE = 
           as.numeric(coefficients(model_ne)[1])
         * as.integer(rownames(cpi_monthly))
         + residuals(model_ne)
         + mean(coefficients(model_ne)[-1]),
         CPI_DS_MW = 
           as.numeric(coefficients(model_mw)[1]) 
         * as.integer(rownames(cpi_monthly))
         + residuals(model_mw)
         + mean(coefficients(model_mw)[-1]),
         CPI_DS_SO = 
           as.numeric(coefficients(model_so)[1]) 
         * as.integer(rownames(cpi_monthly))
         + residuals(model_so)
         + mean(coefficients(model_so)[-1]),
         CPI_DS_WE = 
           as.numeric(coefficients(model_we)[1]) 
         * as.integer(rownames(cpi_monthly))
         + residuals(model_we)
         + mean(coefficients(model_we)[-1]))


rm(list=ls(pattern="^model"))
rm(MonthlySeasonalDummiesLM,
   CPISeasonalityMatrix)

}

if (FALSE)
{

library(ggplot2)
  
plot(fit_ne)
plot(fit_mw)
plot(fit_so)
plot(fit_we)



df_plot <-
  cpi_monthly %>%
  select(DATE,
         CPI_MW,
         CPI_DS_MW) %>% 
  filter(between(DATE,
                 as.Date("2004-06-01"), 
                 as.Date("2016-06-01"))) %>% 
  pivot_longer(-DATE)

plot_obj <- 
  ggplot() + 
  geom_line(data = df_plot,
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(),
        legend.position = "bottom")

plot(plot_obj)


}

######delete up####################
######delete up####################
######delete up####################

## set last date one day back in order not to lose December data later
cpi_monthly <- 
  cpi_monthly %>%
  mutate(DATE = replace(DATE,
                        DATE == "2017-01-01", 
                        "2016-12-31"))


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
         INDEX_CPI_WE = 100 * CPI_WE / CPI_WE[DATE == base_date],
         INDEX_CPI_DS_NE = 100 * CPI_DS_NE / CPI_DS_NE[DATE == base_date],
         INDEX_CPI_DS_MW = 100 * CPI_DS_MW / CPI_DS_MW[DATE == base_date],
         INDEX_CPI_DS_SO = 100 * CPI_DS_SO / CPI_DS_SO[DATE == base_date],
         INDEX_CPI_DS_WE = 100 * CPI_DS_WE / CPI_DS_WE[DATE == base_date])


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
                     INDEX_CPI_DS_NE,
                     INDEX_CPI_MW,
                     INDEX_CPI_DS_MW,
                     INDEX_CPI_SO,
                     INDEX_CPI_DS_SO,
                     INDEX_CPI_WE,
                     INDEX_CPI_DS_WE),
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
                                  na.rm=FALSE),
         INDEX_CPI_DS_NE = na.approx(INDEX_CPI_DS_NE,
                                  na.rm=FALSE),
         INDEX_CPI_DS_MW = na.approx(INDEX_CPI_DS_MW,
                                  na.rm=FALSE),
         INDEX_CPI_DS_SO = na.approx(INDEX_CPI_DS_SO,
                                  na.rm=FALSE),
         INDEX_CPI_DS_WE = na.approx(INDEX_CPI_DS_WE,
                                  na.rm=FALSE))

rm(index_table_monthcpi)

if (FALSE) {
#######################
###### on - TEST HARD CPI pls redo properly
######################

index_table <-
  index_table_monthcpi %>%
  mutate_at(vars(starts_with("INDEX_CPI_")), funs(lead(. , n = 1))) %>% 
  mutate_at(vars(starts_with("INDEX_CPI_")), funs(na.locf(., na.rm=FALSE, fromLast = TRUE))) %>%
  mutate_at(vars(starts_with("INDEX_CPI_")), funs(100*./.[1]))

rm(index_table_monthcpi)
######################
###### off - TEST HARD CPI
######################
}

## Primitives not needed anymore and can be dropped
rm(cpi_monthly,
   stocks_daily,
   tbill_daily)

## Deflates T-Bills and Stock Indexes, cropping last month for
## which there is no inflation data.
index_table <- 
  index_table %>% 
  mutate(INDEX_TB_DEF_NE = 100 * INDEX_TB / INDEX_CPI_NE,
         INDEX_TB_DEF_MW = 100 * INDEX_TB / INDEX_CPI_MW,
         INDEX_TB_DEF_SO = 100 * INDEX_TB / INDEX_CPI_SO,
         INDEX_TB_DEF_WE = 100 * INDEX_TB / INDEX_CPI_WE,
         INDEX_ST_DEF_NE = 100 * INDEX_ST / INDEX_CPI_NE,
         INDEX_ST_DEF_MW = 100 * INDEX_ST / INDEX_CPI_MW,
         INDEX_ST_DEF_SO = 100 * INDEX_ST / INDEX_CPI_SO,
         INDEX_ST_DEF_WE = 100 * INDEX_ST / INDEX_CPI_WE,
         INDEX_TB_DS_DEF_NE = 100 * INDEX_TB / INDEX_CPI_DS_NE,
         INDEX_TB_DS_DEF_MW = 100 * INDEX_TB / INDEX_CPI_DS_MW,
         INDEX_TB_DS_DEF_SO = 100 * INDEX_TB / INDEX_CPI_DS_SO,
         INDEX_TB_DS_DEF_WE = 100 * INDEX_TB / INDEX_CPI_DS_WE,
         INDEX_ST_DS_DEF_NE = 100 * INDEX_ST / INDEX_CPI_DS_NE,
         INDEX_ST_DS_DEF_MW = 100 * INDEX_ST / INDEX_CPI_DS_MW,
         INDEX_ST_DS_DEF_SO = 100 * INDEX_ST / INDEX_CPI_DS_SO,
         INDEX_ST_DS_DEF_WE = 100 * INDEX_ST / INDEX_CPI_DS_WE) %>%
  drop_na()

sprintf("Step %i: Finished Index Deflation", step)
step <- step + 1
rm(step)









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
  

}

if (FALSE) {
  
  df_plot <-
    index_table %>%
    select(DATE,
           INDEX_CPI_MW,
           INDEX_CPI_DS_MW) %>% 
    filter(between(DATE,
                   as.Date("2002-06-01"), 
                   as.Date("2016-06-01"))) %>% 
    pivot_longer(-DATE)
  
  plot_obj <- 
    ggplot() + 
    geom_line(data = df_plot,
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  plot(plot_obj)
  
  
}  



if (FALSE) {
  
  ## coerse data into montly time series object and decompose
  data <- 
    cpi_monthly %>% 
    select(CPI_NE)
  
  ts_cpi_ne <- 
    ts(data$CPI_NE,
       start = c(2003, 1),
       end = c(2016,12),
       frequency = 12)
  
  fit_ne <- 
    stl(ts_cpi_ne, 
        s.window = "period")
  
  data <- 
    cpi_monthly %>% 
    select(CPI_MW)
  
  ts_cpi_mw <- 
    ts(data$CPI_MW,
       start = c(2003, 1),
       end = c(2016,12),
       frequency = 12)
  
  fit_mw <- 
    stl(ts_cpi_mw, 
        s.window = "period")
  
  data <- 
    cpi_monthly %>% 
    select(CPI_SO)
  
  ts_cpi_so <- 
    ts(data$CPI_SO,
       start = c(2003, 1),
       end = c(2016,12),
       frequency = 12)
  
  fit_so <- 
    stl(ts_cpi_so, 
        s.window = "period")
  
  data <- 
    cpi_monthly %>% 
    select(CPI_WE)
  
  ts_cpi_we <- 
    ts(data$CPI_WE,
       start = c(2003, 1),
       end = c(2016,12),
       frequency = 12)
  
  fit_we <- 
    stl(ts_cpi_we, 
        s.window = "period")
  
  ## rejoin deseasonalized data
  
  df_ne <- 
    data.frame(fit_ne$time.series) %>% 
    transmute(CPI_DS_NE = trend+remainder)
  
  df_mw <- 
    data.frame(fit_mw$time.series) %>% 
    transmute(CPI_DS_MW = trend+remainder)
  
  df_so <- 
    data.frame(fit_so$time.series) %>% 
    transmute(CPI_DS_SO = trend+remainder)
  
  df_we <- 
    data.frame(fit_we$time.series) %>% 
    transmute(CPI_DS_WE = trend+remainder)
  
  cpi_monthly <- 
    cpi_monthly %>%
    mutate(CPI_DS_NE = df_ne$CPI_DS_NE,
           CPI_DS_MW = df_mw$CPI_DS_MW,
           CPI_DS_SO = df_so$CPI_DS_SO,
           CPI_DS_WE = df_we$CPI_DS_WE)
  
  rm(data)
  rm(list=ls(pattern="^df"))
  rm(list=ls(pattern="^ts"))
  rm(list=ls(pattern="^fit"))
  
  
  
}