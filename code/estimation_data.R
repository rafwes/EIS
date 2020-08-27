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

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("lead", "dplyr")
conflict_prefer("as.Date", "base")
conflict_prefer("as.Date.numeric", "base")
conflict_prefer("between", "dplyr")

base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code/financial_indexes.R"))
source(file.path(base_path,"EIS/code/consumption_data-grocery_channel-all.R"))

#####################################################################
## DEFLATING AND DESEASONALIZING REGIONAL DAILY CONSUMPTION 
#####################################################################

# Gathers inflation data and deflates consumption by region
DeflateConsumption <- function(x) {
  
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  
  x %>%
    left_join(index_table %>%
                select(DATE, !!INDEX_CPI_REGION),
              by = c("PURCHASE_DATE" = "DATE")) %>% 
    mutate(TOTAL_SPENT_DEF := 
             100 * TOTAL_SPENT 
           / !!INDEX_CPI_REGION) %>% 
    na.exclude() %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT_DEF)
}

# prophet needs a single daily consumption value to deseasonalize.
# first save structure of the panel, then pass mean daily consumption.
# later reconstruct panel from deseasonalized consumption.
DeseasonalizeConsumption <- function(x) {
  
  perc <- 
    x %>% 
    group_by(PURCHASE_DATE) %>% 
    mutate(PERCENTAGE = TOTAL_SPENT_DEF / sum(TOTAL_SPENT_DEF),
           NUM_PANELISTS = n())
  
  data_model <-
    x %>%
    group_by(PURCHASE_DATE) %>%
    summarise(MEAN_SPENT_DAY = sum(TOTAL_SPENT_DEF) / n()) %>%
    rename(ds = PURCHASE_DATE,
           y = MEAN_SPENT_DAY)
  
  # fit model and decompose trend/seasonality
  model <- prophet()
  model_fitted <- fit.prophet(model, data_model)
  data_decomposed <- predict(model_fitted)
  
  # reconstruct de-seasonal trip data from trend and residuals
  data_decomposed %>%
    left_join(data_model, by="ds") %>%
    transmute(PURCHASE_DATE = as.Date(ds),
              DESEASONED = trend + y - yhat
    ) %>%
    left_join(perc, by="PURCHASE_DATE") %>%
    mutate(TOTAL_SPENT_DS_DEF = PERCENTAGE * DESEASONED * NUM_PANELISTS) %>%
    filter(TOTAL_SPENT_DS_DEF > 0) %>%
    select(PURCHASE_DATE,
           HOUSEHOLD_CODE,
           TOTAL_SPENT_DS_DEF)
}

consumption_ds_def_ne <- 
  DeseasonalizeConsumption(
    DeflateConsumption(consumption_ne))
rm(consumption_ne)

consumption_ds_def_mw <- 
  DeseasonalizeConsumption(
    DeflateConsumption(consumption_mw))
rm(consumption_mw)

consumption_ds_def_so <- 
  DeseasonalizeConsumption(
    DeflateConsumption(consumption_so))
rm(consumption_so)

consumption_ds_def_we <- 
  DeseasonalizeConsumption(
    DeflateConsumption(consumption_we))
rm(consumption_we)


#####################################################################
## PREPARING DATA FOR QUARTER GROWTH 
#####################################################################

lag_in_quarters = 1L

# For each quarter, take the average observed t-bill index
# and create a log rate over "lag_in_quarters"
rates_log_avg_qrtly <- 
  index_table %>%
  group_by(YEAR = year(DATE),
           QUARTER = quarter(DATE)) %>% 
  summarise(AVG_INDEX_TB = mean(INDEX_TB),
            AVG_INDEX_CPI_NE = mean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = mean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = mean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = mean(INDEX_CPI_WE),
            AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = mean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = mean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = mean(INDEX_TB_DEF_WE)
            ) %>%
  ungroup() %>% 
  arrange(YEAR,
          QUARTER) %>% 
  transmute(YEAR,
            QUARTER,
            RATE_TB = 
              log(AVG_INDEX_TB) 
            - log(lag(AVG_INDEX_TB,
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
            ) %>%
  na.exclude()

# Sums consumption over given period
# Calculates lagged variables, drops observations for which no
# lags could be calculated and then joins them with rates and
# then delivers a proper date column since.
# Y     = log(C_t) - log(C_{t-1})   ,where C_t is consumption for time t 
# X_TB  = log(1+r)                  ,where r is the real rate for t-bills
# Z1    = Y_{t-2} = log(C_{t-2} - log{C_{t-3}}
# Z2_TB = X_TB_{t-2} 
# Z3    = log(1+\pi)_{t-2}          ,where \pi is the inflation rate
#
# Y calculation is prone to generate NA, therefore it's done earlier.

QuarterlyEstimationData <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)), -2, -1))
  
  RATE_TB_DEF_REGION = as.name(paste0("RATE_TB_DEF_",region))
  RATE_INFL_REGION = as.name(paste0("RATE_INFL_",region))
  
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
              X_TB = !!RATE_TB_DEF_REGION,
              Z1 = Z1,
              Z2_TB = lag(RATE_TB, n = 2), 
              Z3 = lag(!!RATE_INFL_REGION, n = 2),
              ) %>% 
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
                    "csv_output/estimation_data_quarterly_1q.csv"))

zz_tb <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1q,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))


cat("==============================================================\n")
cat("Estimation for 1 Quarter\n")
cat("==============================================================\n")
print(summary(zz_tb))

rm(estimation_data_1q,
   zz_tb, 
   rates_log_avg_qrtly)