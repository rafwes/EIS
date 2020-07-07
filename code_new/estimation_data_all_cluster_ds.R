rm(list=ls())

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(zoo)
library(reshape2)
library(ISOweek)
library(lubridate)
library(prophet)
library(plm)
library(conflicted)
#library(grid)
#library(gridExtra)

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("as.Date", "base")
conflict_prefer("as.Date.numeric", "base")
conflict_prefer("between", "dplyr")

base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
#base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code_new/deflate_then_deseason_ds.R"))

#####################################################################
## ESTIMATIONS FOR 4 WEEKS 
#####################################################################

lag_in_weeks = 4L

# For each week, take the average observed tbill/stock index
# and create a log rate over "lag_in_weeks"
rates_log_avg_wkly <- 
  index_table %>%
  group_by(ISOWEEK = ISOweek(DATE)) %>% 
  summarise(AVG_INDEX_TB = mean(INDEX_TB),
            AVG_INDEX_ST = mean(INDEX_ST),
            AVG_INDEX_CPI_NE = mean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = mean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = mean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = mean(INDEX_CPI_WE),
            AVG_INDEX_CPI_DS_NE = mean(INDEX_CPI_DS_NE),
            AVG_INDEX_CPI_DS_MW = mean(INDEX_CPI_DS_MW),
            AVG_INDEX_CPI_DS_SO = mean(INDEX_CPI_DS_SO),
            AVG_INDEX_CPI_DS_WE = mean(INDEX_CPI_DS_WE),
            AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = mean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = mean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = mean(INDEX_TB_DEF_WE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE),
            AVG_INDEX_ST_DEF_MW = mean(INDEX_ST_DEF_MW),
            AVG_INDEX_ST_DEF_SO = mean(INDEX_ST_DEF_SO),
            AVG_INDEX_ST_DEF_WE = mean(INDEX_ST_DEF_WE),
            AVG_INDEX_TB_DS_DEF_NE = mean(INDEX_TB_DS_DEF_NE),
            AVG_INDEX_TB_DS_DEF_MW = mean(INDEX_TB_DS_DEF_MW),
            AVG_INDEX_TB_DS_DEF_SO = mean(INDEX_TB_DS_DEF_SO),
            AVG_INDEX_TB_DS_DEF_WE = mean(INDEX_TB_DS_DEF_WE),
            AVG_INDEX_ST_DS_DEF_NE = mean(INDEX_ST_DS_DEF_NE),
            AVG_INDEX_ST_DS_DEF_MW = mean(INDEX_ST_DS_DEF_MW),
            AVG_INDEX_ST_DS_DEF_SO = mean(INDEX_ST_DS_DEF_SO),
            AVG_INDEX_ST_DS_DEF_WE = mean(INDEX_ST_DS_DEF_WE)) %>%
  ungroup() %>% 
  arrange(ISOWEEK) %>% 
  transmute(ISOWEEK = ISOWEEK,
            RATE_TB = 
              log(AVG_INDEX_TB) 
            - log(lag(AVG_INDEX_TB,
                      n = lag_in_weeks)),
            RATE_ST = 
              log(AVG_INDEX_ST) 
            - log(lag(AVG_INDEX_ST,
                      n = lag_in_weeks)),
            RATE_INFL_NE =
              log(AVG_INDEX_CPI_NE) 
            - log(lag(AVG_INDEX_CPI_NE,
                      n = lag_in_weeks)),
            RATE_INFL_MW =
              log(AVG_INDEX_CPI_MW) 
            - log(lag(AVG_INDEX_CPI_MW,
                      n = lag_in_weeks)),
            RATE_INFL_SO =
              log(AVG_INDEX_CPI_SO) 
            - log(lag(AVG_INDEX_CPI_SO,
                      n = lag_in_weeks)),
            RATE_INFL_WE =
              log(AVG_INDEX_CPI_WE) 
            - log(lag(AVG_INDEX_CPI_WE,
                      n = lag_in_weeks)),
            RATE_INFL_DS_NE =
              log(AVG_INDEX_CPI_DS_NE) 
            - log(lag(AVG_INDEX_CPI_DS_NE,
                      n = lag_in_weeks)),
            RATE_INFL_DS_MW =
              log(AVG_INDEX_CPI_DS_MW) 
            - log(lag(AVG_INDEX_CPI_DS_MW,
                      n = lag_in_weeks)),
            RATE_INFL_DS_SO =
              log(AVG_INDEX_CPI_DS_SO) 
            - log(lag(AVG_INDEX_CPI_DS_SO,
                      n = lag_in_weeks)),
            RATE_INFL_DS_WE =
              log(AVG_INDEX_CPI_DS_WE) 
            - log(lag(AVG_INDEX_CPI_DS_WE,
                      n = lag_in_weeks)),
            RATE_TB_DEF_NE = 
              log(AVG_INDEX_TB_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DEF_NE,
                      n = lag_in_weeks)),
            RATE_TB_DEF_MW = 
              log(AVG_INDEX_TB_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DEF_MW,
                      n = lag_in_weeks)),
            RATE_TB_DEF_SO = 
              log(AVG_INDEX_TB_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DEF_SO,
                      n = lag_in_weeks)),
            RATE_TB_DEF_WE = 
              log(AVG_INDEX_TB_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DEF_WE,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_NE = 
              log(AVG_INDEX_TB_DS_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_NE,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_MW = 
              log(AVG_INDEX_TB_DS_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DS_DEF_MW,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_SO = 
              log(AVG_INDEX_TB_DS_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DS_DEF_SO,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_WE = 
              log(AVG_INDEX_TB_DS_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_WE,
                      n = lag_in_weeks)),
            RATE_ST_DEF_NE = 
              log(AVG_INDEX_ST_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DEF_NE,
                      n = lag_in_weeks)),
            RATE_ST_DEF_MW = 
              log(AVG_INDEX_ST_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DEF_MW,
                      n = lag_in_weeks)),
            RATE_ST_DEF_SO = 
              log(AVG_INDEX_ST_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DEF_SO,
                      n = lag_in_weeks)),
            RATE_ST_DEF_WE = 
              log(AVG_INDEX_ST_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DEF_WE,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_NE = 
              log(AVG_INDEX_ST_DS_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_NE,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_MW = 
              log(AVG_INDEX_ST_DS_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DS_DEF_MW,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_SO = 
              log(AVG_INDEX_ST_DS_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DS_DEF_SO,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_WE = 
              log(AVG_INDEX_ST_DS_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_WE,
                      n = lag_in_weeks))
  ) %>%
  na.exclude()

# Sums consumption over given period
# Calculates lagged variables, drops observations for which no
# lags could be calculated and then joins them with rates and
# then delivers a proper date column since.
# Y     = log(C_t) - log(C_{t-4})   ,where C_t is consumption for time t 
# X_TB  = log(1+r)                  ,where r is the real rate for t-bills
# X_ST  = log(1+r)                  ,same for stock returns
# Z1    = Y_{t-2} = log(C_{t-2} - log{C_{t-6}}
# Z2_TB = X_TB_{t-2} 
# Z2_ST = X_ST_{t-2}
# Z3    = log(1+\pi)_{t-2}          ,where \pi is the inflation rate
#
# Y calculation is prone to generate NA, therefore it's done earlier.

WeeklyEstimationData <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)), -2, -1))
  
  RATE_TB_DS_DEF_REGION = as.name(paste0("RATE_TB_DS_DEF_",region))
  RATE_ST_DS_DEF_REGION = as.name(paste0("RATE_ST_DS_DEF_",region))
  RATE_INFL_DS_REGION = as.name(paste0("RATE_INFL_DS_",region))
  
  x %>%
    group_by(HOUSEHOLD_CODE,
             ISOWEEK = ISOweek(PURCHASE_DATE)) %>% 
    summarise(SUM_SPENT_DS_DEF = 
                sum(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>%
    filter(SUM_SPENT_DS_DEF > 0) %>%
    complete(ISOWEEK,
             HOUSEHOLD_CODE) %>%
    group_by(HOUSEHOLD_CODE) %>%
    arrange(ISOWEEK) %>%
    mutate(Y = 
             log(SUM_SPENT_DS_DEF) 
           - log(lag(SUM_SPENT_DS_DEF,
                     n = lag_in_weeks)),
           Z1 = 
             lag(Y, n = 2)) %>%
    #na.exclude() %>%
    left_join(rates_log_avg_wkly,
              by = "ISOWEEK") %>%
    transmute(DATE = as.Date(ISOweek2date(paste(ISOWEEK, "1", sep = "-"))),
              Y = Y,
              X_TB = !!RATE_TB_DS_DEF_REGION,
              X_ST = !!RATE_ST_DS_DEF_REGION,
              Z1 = Z1,
              Z2_TB = lag(RATE_TB, n = 2), 
              Z2_ST = lag(RATE_ST, n = 2),
              Z3 = lag(!!RATE_INFL_DS_REGION, n = 2)) %>% 
    na.exclude() %>% 
    ungroup() %>%
    rename(HOUSEHOLD = HOUSEHOLD_CODE)
  
}

estimation_data_4w <-
  bind_rows(WeeklyEstimationData(consumption_ds_def_ne),
            WeeklyEstimationData(consumption_ds_def_mw),
            WeeklyEstimationData(consumption_ds_def_so),
            WeeklyEstimationData(consumption_ds_def_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_4w,
          file.path(base_path, 
                    "csv_output/estimation_data_weekly_4w.csv"))

zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_4w,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 4 Weeks")
print("======================================================")
print(summary(zz))
rm(estimation_data_4w,
   zz,
   rates_log_avg_wkly)


#####################################################################
## ESTIMATIONS FOR 1 WEEK 
#####################################################################

lag_in_weeks = 1L

# For each week, take the average observed tbill/stock index
# and create a log rate over "lag_in_weeks"
rates_log_avg_wkly <- 
  index_table %>%
  group_by(ISOWEEK = ISOweek(DATE)) %>% 
  summarise(AVG_INDEX_TB = mean(INDEX_TB),
            AVG_INDEX_ST = mean(INDEX_ST),
            AVG_INDEX_CPI_NE = mean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = mean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = mean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = mean(INDEX_CPI_WE),
            AVG_INDEX_CPI_DS_NE = mean(INDEX_CPI_DS_NE),
            AVG_INDEX_CPI_DS_MW = mean(INDEX_CPI_DS_MW),
            AVG_INDEX_CPI_DS_SO = mean(INDEX_CPI_DS_SO),
            AVG_INDEX_CPI_DS_WE = mean(INDEX_CPI_DS_WE),
            AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = mean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = mean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = mean(INDEX_TB_DEF_WE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE),
            AVG_INDEX_ST_DEF_MW = mean(INDEX_ST_DEF_MW),
            AVG_INDEX_ST_DEF_SO = mean(INDEX_ST_DEF_SO),
            AVG_INDEX_ST_DEF_WE = mean(INDEX_ST_DEF_WE),
            AVG_INDEX_TB_DS_DEF_NE = mean(INDEX_TB_DS_DEF_NE),
            AVG_INDEX_TB_DS_DEF_MW = mean(INDEX_TB_DS_DEF_MW),
            AVG_INDEX_TB_DS_DEF_SO = mean(INDEX_TB_DS_DEF_SO),
            AVG_INDEX_TB_DS_DEF_WE = mean(INDEX_TB_DS_DEF_WE),
            AVG_INDEX_ST_DS_DEF_NE = mean(INDEX_ST_DS_DEF_NE),
            AVG_INDEX_ST_DS_DEF_MW = mean(INDEX_ST_DS_DEF_MW),
            AVG_INDEX_ST_DS_DEF_SO = mean(INDEX_ST_DS_DEF_SO),
            AVG_INDEX_ST_DS_DEF_WE = mean(INDEX_ST_DS_DEF_WE)) %>%
  ungroup() %>% 
  arrange(ISOWEEK) %>% 
  transmute(ISOWEEK = ISOWEEK,
            RATE_TB = 
              log(AVG_INDEX_TB) 
            - log(lag(AVG_INDEX_TB,
                      n = lag_in_weeks)),
            RATE_ST = 
              log(AVG_INDEX_ST) 
            - log(lag(AVG_INDEX_ST,
                      n = lag_in_weeks)),
            RATE_INFL_NE =
              log(AVG_INDEX_CPI_NE) 
            - log(lag(AVG_INDEX_CPI_NE,
                      n = lag_in_weeks)),
            RATE_INFL_MW =
              log(AVG_INDEX_CPI_MW) 
            - log(lag(AVG_INDEX_CPI_MW,
                      n = lag_in_weeks)),
            RATE_INFL_SO =
              log(AVG_INDEX_CPI_SO) 
            - log(lag(AVG_INDEX_CPI_SO,
                      n = lag_in_weeks)),
            RATE_INFL_WE =
              log(AVG_INDEX_CPI_WE) 
            - log(lag(AVG_INDEX_CPI_WE,
                      n = lag_in_weeks)),
            RATE_INFL_DS_NE =
              log(AVG_INDEX_CPI_DS_NE) 
            - log(lag(AVG_INDEX_CPI_DS_NE,
                      n = lag_in_weeks)),
            RATE_INFL_DS_MW =
              log(AVG_INDEX_CPI_DS_MW) 
            - log(lag(AVG_INDEX_CPI_DS_MW,
                      n = lag_in_weeks)),
            RATE_INFL_DS_SO =
              log(AVG_INDEX_CPI_DS_SO) 
            - log(lag(AVG_INDEX_CPI_DS_SO,
                      n = lag_in_weeks)),
            RATE_INFL_DS_WE =
              log(AVG_INDEX_CPI_DS_WE) 
            - log(lag(AVG_INDEX_CPI_DS_WE,
                      n = lag_in_weeks)),
            RATE_TB_DEF_NE = 
              log(AVG_INDEX_TB_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DEF_NE,
                      n = lag_in_weeks)),
            RATE_TB_DEF_MW = 
              log(AVG_INDEX_TB_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DEF_MW,
                      n = lag_in_weeks)),
            RATE_TB_DEF_SO = 
              log(AVG_INDEX_TB_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DEF_SO,
                      n = lag_in_weeks)),
            RATE_TB_DEF_WE = 
              log(AVG_INDEX_TB_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DEF_WE,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_NE = 
              log(AVG_INDEX_TB_DS_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_NE,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_MW = 
              log(AVG_INDEX_TB_DS_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DS_DEF_MW,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_SO = 
              log(AVG_INDEX_TB_DS_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DS_DEF_SO,
                      n = lag_in_weeks)),
            RATE_TB_DS_DEF_WE = 
              log(AVG_INDEX_TB_DS_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_WE,
                      n = lag_in_weeks)),
            RATE_ST_DEF_NE = 
              log(AVG_INDEX_ST_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DEF_NE,
                      n = lag_in_weeks)),
            RATE_ST_DEF_MW = 
              log(AVG_INDEX_ST_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DEF_MW,
                      n = lag_in_weeks)),
            RATE_ST_DEF_SO = 
              log(AVG_INDEX_ST_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DEF_SO,
                      n = lag_in_weeks)),
            RATE_ST_DEF_WE = 
              log(AVG_INDEX_ST_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DEF_WE,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_NE = 
              log(AVG_INDEX_ST_DS_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_NE,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_MW = 
              log(AVG_INDEX_ST_DS_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DS_DEF_MW,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_SO = 
              log(AVG_INDEX_ST_DS_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DS_DEF_SO,
                      n = lag_in_weeks)),
            RATE_ST_DS_DEF_WE = 
              log(AVG_INDEX_ST_DS_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_WE,
                      n = lag_in_weeks))
  ) %>%
  na.exclude()

estimation_data_1w <-
  bind_rows(WeeklyEstimationData(consumption_ds_def_ne),
            WeeklyEstimationData(consumption_ds_def_mw),
            WeeklyEstimationData(consumption_ds_def_so),
            WeeklyEstimationData(consumption_ds_def_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_1w,
          file.path(base_path, 
                    "csv_output/estimation_data_weekly_1w.csv"))

zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1w,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 1 Week")
print("======================================================")
print(summary(zz))
rm(estimation_data_1w,
   zz,
   rates_log_avg_wkly)




#####################################################################
## ESTIMATIONS FOR 1 MONTH 
#####################################################################

lag_in_months = 1L

# For each week, take the average observed tbill/stock index
# and create a log rate over over "lag_in_months"
rates_log_avg_mthly <- 
  index_table %>%
  group_by(YEAR = year(DATE),
           MONTH = month(DATE)) %>% 
  summarise(AVG_INDEX_TB = mean(INDEX_TB),
            AVG_INDEX_ST = mean(INDEX_ST),
            AVG_INDEX_CPI_NE = mean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = mean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = mean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = mean(INDEX_CPI_WE),
            AVG_INDEX_CPI_DS_NE = mean(INDEX_CPI_DS_NE),
            AVG_INDEX_CPI_DS_MW = mean(INDEX_CPI_DS_MW),
            AVG_INDEX_CPI_DS_SO = mean(INDEX_CPI_DS_SO),
            AVG_INDEX_CPI_DS_WE = mean(INDEX_CPI_DS_WE),
            AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = mean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = mean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = mean(INDEX_TB_DEF_WE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE),
            AVG_INDEX_ST_DEF_MW = mean(INDEX_ST_DEF_MW),
            AVG_INDEX_ST_DEF_SO = mean(INDEX_ST_DEF_SO),
            AVG_INDEX_ST_DEF_WE = mean(INDEX_ST_DEF_WE),
            AVG_INDEX_TB_DS_DEF_NE = mean(INDEX_TB_DS_DEF_NE),
            AVG_INDEX_TB_DS_DEF_MW = mean(INDEX_TB_DS_DEF_MW),
            AVG_INDEX_TB_DS_DEF_SO = mean(INDEX_TB_DS_DEF_SO),
            AVG_INDEX_TB_DS_DEF_WE = mean(INDEX_TB_DS_DEF_WE),
            AVG_INDEX_ST_DS_DEF_NE = mean(INDEX_ST_DS_DEF_NE),
            AVG_INDEX_ST_DS_DEF_MW = mean(INDEX_ST_DS_DEF_MW),
            AVG_INDEX_ST_DS_DEF_SO = mean(INDEX_ST_DS_DEF_SO),
            AVG_INDEX_ST_DS_DEF_WE = mean(INDEX_ST_DS_DEF_WE)) %>%
  ungroup() %>%
  arrange(YEAR,
          MONTH) %>%
  transmute(YEAR = YEAR,
            MONTH = MONTH,
            RATE_TB = 
              log(AVG_INDEX_TB) 
            - log(lag(AVG_INDEX_TB,
                      n = lag_in_months)),
            RATE_ST = 
              log(AVG_INDEX_ST) 
            - log(lag(AVG_INDEX_ST,
                      n = lag_in_months)),
            RATE_INFL_NE =
              log(AVG_INDEX_CPI_NE) 
            - log(lag(AVG_INDEX_CPI_NE,
                      n = lag_in_months)),
            RATE_INFL_MW =
              log(AVG_INDEX_CPI_MW) 
            - log(lag(AVG_INDEX_CPI_MW,
                      n = lag_in_months)),
            RATE_INFL_SO =
              log(AVG_INDEX_CPI_SO) 
            - log(lag(AVG_INDEX_CPI_SO,
                      n = lag_in_months)),
            RATE_INFL_WE =
              log(AVG_INDEX_CPI_WE) 
            - log(lag(AVG_INDEX_CPI_WE,
                      n = lag_in_months)),
            RATE_INFL_DS_NE =
              log(AVG_INDEX_CPI_DS_NE) 
            - log(lag(AVG_INDEX_CPI_DS_NE,
                      n = lag_in_months)),
            RATE_INFL_DS_MW =
              log(AVG_INDEX_CPI_DS_MW) 
            - log(lag(AVG_INDEX_CPI_DS_MW,
                      n = lag_in_months)),
            RATE_INFL_DS_SO =
              log(AVG_INDEX_CPI_DS_SO) 
            - log(lag(AVG_INDEX_CPI_DS_SO,
                      n = lag_in_months)),
            RATE_INFL_DS_WE =
              log(AVG_INDEX_CPI_DS_WE) 
            - log(lag(AVG_INDEX_CPI_DS_WE,
                      n = lag_in_months)),
            RATE_TB_DEF_NE = 
              log(AVG_INDEX_TB_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DEF_NE,
                      n = lag_in_months)),
            RATE_TB_DEF_MW = 
              log(AVG_INDEX_TB_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DEF_MW,
                      n = lag_in_months)),
            RATE_TB_DEF_SO = 
              log(AVG_INDEX_TB_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DEF_SO,
                      n = lag_in_months)),
            RATE_TB_DEF_WE = 
              log(AVG_INDEX_TB_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DEF_WE,
                      n = lag_in_months)),
            RATE_TB_DS_DEF_NE = 
              log(AVG_INDEX_TB_DS_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_NE,
                      n = lag_in_months)),
            RATE_TB_DS_DEF_MW = 
              log(AVG_INDEX_TB_DS_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DS_DEF_MW,
                      n = lag_in_months)),
            RATE_TB_DS_DEF_SO = 
              log(AVG_INDEX_TB_DS_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DS_DEF_SO,
                      n = lag_in_months)),
            RATE_TB_DS_DEF_WE = 
              log(AVG_INDEX_TB_DS_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_WE,
                      n = lag_in_months)),
            RATE_ST_DEF_NE = 
              log(AVG_INDEX_ST_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DEF_NE,
                      n = lag_in_months)),
            RATE_ST_DEF_MW = 
              log(AVG_INDEX_ST_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DEF_MW,
                      n = lag_in_months)),
            RATE_ST_DEF_SO = 
              log(AVG_INDEX_ST_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DEF_SO,
                      n = lag_in_months)),
            RATE_ST_DEF_WE = 
              log(AVG_INDEX_ST_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DEF_WE,
                      n = lag_in_months)),
            RATE_ST_DS_DEF_NE = 
              log(AVG_INDEX_ST_DS_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_NE,
                      n = lag_in_months)),
            RATE_ST_DS_DEF_MW = 
              log(AVG_INDEX_ST_DS_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DS_DEF_MW,
                      n = lag_in_months)),
            RATE_ST_DS_DEF_SO = 
              log(AVG_INDEX_ST_DS_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DS_DEF_SO,
                      n = lag_in_months)),
            RATE_ST_DS_DEF_WE = 
              log(AVG_INDEX_ST_DS_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_WE,
                      n = lag_in_months))
  ) %>%
  na.exclude()


MonthlyEstimationData <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)), -2, -1))
  
  RATE_TB_DS_DEF_REGION = as.name(paste0("RATE_TB_DS_DEF_",region))
  RATE_ST_DS_DEF_REGION = as.name(paste0("RATE_ST_DS_DEF_",region))
  RATE_INFL_DS_REGION = as.name(paste0("RATE_INFL_DS_",region))
  
  x %>%
    group_by(HOUSEHOLD_CODE,
             YEAR = year(PURCHASE_DATE),
             MONTH = month(PURCHASE_DATE)) %>% 
    summarise(SUM_SPENT_DS_DEF = 
                sum(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>%
    filter(SUM_SPENT_DS_DEF > 0) %>%
    complete(YEAR,
             MONTH,
             HOUSEHOLD_CODE) %>%
    group_by(HOUSEHOLD_CODE) %>%
    arrange(YEAR, MONTH) %>%
    mutate(Y = 
             log(SUM_SPENT_DS_DEF) 
           - log(lag(SUM_SPENT_DS_DEF,
                     n = lag_in_months)),
           Z1 = 
             lag(Y, n = 2)) %>%
    #na.exclude() %>%
    left_join(rates_log_avg_mthly,
              by = c("YEAR","MONTH")) %>%
    unite(YEAR_MONTH,
          YEAR,MONTH,
          sep = "-") %>%
    transmute(DATE = as.Date(paste(YEAR_MONTH, "1", sep = "-")),
              Y = Y,
              X_TB = !!RATE_TB_DS_DEF_REGION,
              X_ST = !!RATE_ST_DS_DEF_REGION,
              Z1 = Z1,
              Z2_TB = lag(RATE_TB, n = 2), 
              Z2_ST = lag(RATE_ST, n = 2),
              Z3 = lag(!!RATE_INFL_DS_REGION, n = 2)) %>% 
    na.exclude() %>% 
    ungroup() %>%
    rename(HOUSEHOLD = HOUSEHOLD_CODE)
  
}

estimation_data_1m <-
  bind_rows(MonthlyEstimationData(consumption_ds_def_ne),
            MonthlyEstimationData(consumption_ds_def_mw),
            MonthlyEstimationData(consumption_ds_def_so),
            MonthlyEstimationData(consumption_ds_def_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_1m,
          file.path(base_path, 
                    "csv_output/estimation_data_monthly_1m.csv"))

zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1m,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 1 Month")
print("======================================================")
print(summary(zz))
rm(estimation_data_1m,
   zz,
   rates_log_avg_mthly)


#####################################################################
## ESTIMATIONS FOR 1 QUARTER 
#####################################################################


lag_in_quarters = 1L

# For each week, take the average observed tbill/stock index
# and create a log rate over "lag_in_quarters"
rates_log_avg_qrtly <- 
  index_table %>%
  group_by(YEAR = year(DATE),
           QUARTER = quarter(DATE)) %>% 
  summarise(AVG_INDEX_TB = mean(INDEX_TB),
            AVG_INDEX_ST = mean(INDEX_ST),
            AVG_INDEX_CPI_NE = mean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = mean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = mean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = mean(INDEX_CPI_WE),
            AVG_INDEX_CPI_DS_NE = mean(INDEX_CPI_DS_NE),
            AVG_INDEX_CPI_DS_MW = mean(INDEX_CPI_DS_MW),
            AVG_INDEX_CPI_DS_SO = mean(INDEX_CPI_DS_SO),
            AVG_INDEX_CPI_DS_WE = mean(INDEX_CPI_DS_WE),
            AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = mean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = mean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = mean(INDEX_TB_DEF_WE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE),
            AVG_INDEX_ST_DEF_MW = mean(INDEX_ST_DEF_MW),
            AVG_INDEX_ST_DEF_SO = mean(INDEX_ST_DEF_SO),
            AVG_INDEX_ST_DEF_WE = mean(INDEX_ST_DEF_WE),
            AVG_INDEX_TB_DS_DEF_NE = mean(INDEX_TB_DS_DEF_NE),
            AVG_INDEX_TB_DS_DEF_MW = mean(INDEX_TB_DS_DEF_MW),
            AVG_INDEX_TB_DS_DEF_SO = mean(INDEX_TB_DS_DEF_SO),
            AVG_INDEX_TB_DS_DEF_WE = mean(INDEX_TB_DS_DEF_WE),
            AVG_INDEX_ST_DS_DEF_NE = mean(INDEX_ST_DS_DEF_NE),
            AVG_INDEX_ST_DS_DEF_MW = mean(INDEX_ST_DS_DEF_MW),
            AVG_INDEX_ST_DS_DEF_SO = mean(INDEX_ST_DS_DEF_SO),
            AVG_INDEX_ST_DS_DEF_WE = mean(INDEX_ST_DS_DEF_WE)) %>%
  ungroup() %>% 
  arrange(YEAR,
          QUARTER) %>% 
  transmute(YEAR,
            QUARTER,
            RATE_TB = 
              log(AVG_INDEX_TB) 
            - log(lag(AVG_INDEX_TB,
                      n = lag_in_quarters)),
            RATE_ST = 
              log(AVG_INDEX_ST) 
            - log(lag(AVG_INDEX_ST,
                      n = lag_in_quarters)),
            RATE_INFL_NE =
              log(AVG_INDEX_CPI_NE) 
            - log(lag(AVG_INDEX_CPI_NE,
                      n = lag_in_quarters)),
            RATE_INFL_MW =
              log(AVG_INDEX_CPI_MW) 
            - log(lag(AVG_INDEX_CPI_MW,
                      n = lag_in_quarters)),
            RATE_INFL_SO =
              log(AVG_INDEX_CPI_SO) 
            - log(lag(AVG_INDEX_CPI_SO,
                      n = lag_in_quarters)),
            RATE_INFL_WE =
              log(AVG_INDEX_CPI_WE) 
            - log(lag(AVG_INDEX_CPI_WE,
                      n = lag_in_quarters)),
            RATE_INFL_DS_NE =
              log(AVG_INDEX_CPI_DS_NE) 
            - log(lag(AVG_INDEX_CPI_DS_NE,
                      n = lag_in_quarters)),
            RATE_INFL_DS_MW =
              log(AVG_INDEX_CPI_DS_MW) 
            - log(lag(AVG_INDEX_CPI_DS_MW,
                      n = lag_in_quarters)),
            RATE_INFL_DS_SO =
              log(AVG_INDEX_CPI_DS_SO) 
            - log(lag(AVG_INDEX_CPI_DS_SO,
                      n = lag_in_quarters)),
            RATE_INFL_DS_WE =
              log(AVG_INDEX_CPI_DS_WE) 
            - log(lag(AVG_INDEX_CPI_DS_WE,
                      n = lag_in_quarters)),
            RATE_TB_DEF_NE = 
              log(AVG_INDEX_TB_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DEF_NE,
                      n = lag_in_quarters)),
            RATE_TB_DEF_MW = 
              log(AVG_INDEX_TB_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DEF_MW,
                      n = lag_in_quarters)),
            RATE_TB_DEF_SO = 
              log(AVG_INDEX_TB_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DEF_SO,
                      n = lag_in_quarters)),
            RATE_TB_DEF_WE = 
              log(AVG_INDEX_TB_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DEF_WE,
                      n = lag_in_quarters)),
            RATE_TB_DS_DEF_NE = 
              log(AVG_INDEX_TB_DS_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_NE,
                      n = lag_in_quarters)),
            RATE_TB_DS_DEF_MW = 
              log(AVG_INDEX_TB_DS_DEF_MW) 
            - log(lag(AVG_INDEX_TB_DS_DEF_MW,
                      n = lag_in_quarters)),
            RATE_TB_DS_DEF_SO = 
              log(AVG_INDEX_TB_DS_DEF_SO) 
            - log(lag(AVG_INDEX_TB_DS_DEF_SO,
                      n = lag_in_quarters)),
            RATE_TB_DS_DEF_WE = 
              log(AVG_INDEX_TB_DS_DEF_WE) 
            - log(lag(AVG_INDEX_TB_DS_DEF_WE,
                      n = lag_in_quarters)),
            RATE_ST_DEF_NE = 
              log(AVG_INDEX_ST_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DEF_NE,
                      n = lag_in_quarters)),
            RATE_ST_DEF_MW = 
              log(AVG_INDEX_ST_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DEF_MW,
                      n = lag_in_quarters)),
            RATE_ST_DEF_SO = 
              log(AVG_INDEX_ST_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DEF_SO,
                      n = lag_in_quarters)),
            RATE_ST_DEF_WE = 
              log(AVG_INDEX_ST_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DEF_WE,
                      n = lag_in_quarters)),
            RATE_ST_DS_DEF_NE = 
              log(AVG_INDEX_ST_DS_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_NE,
                      n = lag_in_quarters)),
            RATE_ST_DS_DEF_MW = 
              log(AVG_INDEX_ST_DS_DEF_MW) 
            - log(lag(AVG_INDEX_ST_DS_DEF_MW,
                      n = lag_in_quarters)),
            RATE_ST_DS_DEF_SO = 
              log(AVG_INDEX_ST_DS_DEF_SO) 
            - log(lag(AVG_INDEX_ST_DS_DEF_SO,
                      n = lag_in_quarters)),
            RATE_ST_DS_DEF_WE = 
              log(AVG_INDEX_ST_DS_DEF_WE) 
            - log(lag(AVG_INDEX_ST_DS_DEF_WE,
                      n = lag_in_quarters))
  ) %>%
  na.exclude()

# Sums consumption over given period
# Calculates lagged variables, drops observations for which no
# lags could be calculated and then joins them with rates and
# then delivers a proper date column since.
# Y     = log(C_t) - log(C_{t-1})   ,where C_t is consumption for time t 
# X_TB  = log(1+r)                  ,where r is the real rate for t-bills
# X_ST  = log(1+r)                  ,same for stock returns
# Z1    = Y_{t-2} = log(C_{t-2} - log{C_{t-3}}
# Z2_TB = X_TB_{t-2} 
# Z2_ST = X_ST_{t-2}
# Z3    = log(1+\pi)_{t-2}          ,where \pi is the inflation rate
#
# Y calculation is prone to generate NA, therefore it's done earlier.

QuarterlyEstimationData <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)), -2, -1))
  
  RATE_TB_DS_DEF_REGION = as.name(paste0("RATE_TB_DS_DEF_",region))
  RATE_ST_DS_DEF_REGION = as.name(paste0("RATE_ST_DS_DEF_",region))
  RATE_INFL_DS_REGION = as.name(paste0("RATE_INFL_DS_",region))
  
  x %>%
    group_by(HOUSEHOLD_CODE,
             YEAR = year(PURCHASE_DATE),
             QUARTER = quarter(PURCHASE_DATE)) %>% 
    summarise(SUM_SPENT_DS_DEF = 
                sum(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>%
    filter(SUM_SPENT_DS_DEF > 0) %>%
    complete(YEAR,
             QUARTER,
             HOUSEHOLD_CODE) %>%
    group_by(HOUSEHOLD_CODE) %>%
    arrange(YEAR, QUARTER) %>%
    mutate(Y = 
             log(SUM_SPENT_DS_DEF) 
           - log(lag(SUM_SPENT_DS_DEF,
                     n = lag_in_quarters)),
           Z1 = 
             lag(Y, n = 2)) %>%
    #na.exclude() %>%
    left_join(rates_log_avg_qrtly,
              by = c("YEAR","QUARTER")) %>%
    mutate(MONTH = case_when(QUARTER == 1 ~ 1,
                             QUARTER == 2 ~ 4,
                             QUARTER == 3 ~ 7,
                             QUARTER == 4 ~ 10),
           DAY = 1) %>%
    unite(CHAR_DATE,
          YEAR,MONTH,DAY,
          sep = "-") %>% 
    transmute(DATE = as.Date(CHAR_DATE),
              Y = Y,
              X_TB = !!RATE_TB_DS_DEF_REGION,
              X_ST = !!RATE_ST_DS_DEF_REGION,
              Z1 = Z1,
              Z2_TB = lag(RATE_TB, n = 2), 
              Z2_ST = lag(RATE_ST, n = 2),
              Z3 = lag(!!RATE_INFL_DS_REGION, n = 2)) %>% 
    na.exclude() %>% 
    ungroup() %>%
    rename(HOUSEHOLD = HOUSEHOLD_CODE) 
  
}

estimation_data_1q <-
  bind_rows(QuarterlyEstimationData(consumption_ds_def_ne),
            QuarterlyEstimationData(consumption_ds_def_mw),
            QuarterlyEstimationData(consumption_ds_def_so),
            QuarterlyEstimationData(consumption_ds_def_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_1q,
          file.path(base_path, 
                    "csv_output/estimation_data_quarterly_ds_1q.csv"))

zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1q,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 1 Quarter")
print("======================================================")
print(summary(zz))
rm(estimation_data_1q,
   zz,
   rates_log_avg_qrtly)
