rm(list=ls())

library(tidyverse)
library(zoo)
library(reshape2)
library(naniar)
library(visdat)
library(ISOweek)
library(lubridate)
library(EnvStats)

#base_path <- "/extra/agalvao/eis_nielsen"
base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code_new/interest_rates.R"))
source(file.path(base_path,"EIS/code_new/grocery_data.R"))

lag_in_weeks = 4L

# For each week, take the average observed tbill/stock index
# and create a log rate over "lag_in_weeks"
rates_log_avg_ne <- 
  index_table %>%
  group_by(ISOWEEK = ISOweek(DATE)) %>% 
  summarise(AVG_INDEX_TB = geoMean(INDEX_TB),
            AVG_INDEX_ST = geoMean(INDEX_ST),
            AVG_INDEX_CPI_NE = geoMean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = geoMean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = geoMean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = geoMean(INDEX_CPI_WE),
            AVG_INDEX_TB_DEF_NE = geoMean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = geoMean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = geoMean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = geoMean(INDEX_TB_DEF_WE),
            AVG_INDEX_ST_DEF_NE = geoMean(INDEX_ST_DEF_NE),
            AVG_INDEX_ST_DEF_MW = geoMean(INDEX_ST_DEF_MW),
            AVG_INDEX_ST_DEF_SO = geoMean(INDEX_ST_DEF_SO),
            AVG_INDEX_ST_DEF_WE = geoMean(INDEX_ST_DEF_WE)) %>%
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
                      n = lag_in_weeks))
            ) %>%
  na.exclude()


# Gathers inflation data and deflates consumption by region
# Consumption data is too sparse, condense into weekly data
Deflate_Than_Sum <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  
  TOTAL_SPENT_DEF_REGION = as.name(paste0("TOTAL_SPENT_DEF_",region))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  SUM_SPENT_DEF_REGION = as.name(paste0("SUM_SPENT_DEF_",region))
  
  x %>%
    left_join(index_table %>%
                select(DATE,!!INDEX_CPI_REGION),
              by = c("PURCHASE_DATE" = "DATE")) %>% 
    mutate(!!TOTAL_SPENT_DEF_REGION := 
             100 * TOTAL_SPENT 
           / !!INDEX_CPI_REGION) %>% 
    na.exclude() %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           !!TOTAL_SPENT_DEF_REGION) %>% 
    group_by(HOUSEHOLD_CODE,
             ISOWEEK = ISOweek(PURCHASE_DATE)) %>% 
    summarise(!!SUM_SPENT_DEF_REGION := 
                sum(!!TOTAL_SPENT_DEF_REGION)) %>%
    ungroup()
}


sum_consumption_ne_def <- Deflate_Than_Sum(consumption_ne)
rm(consumption_ne)

sum_consumption_mw_def <- Deflate_Than_Sum(consumption_mw)
rm(consumption_mw)

sum_consumption_so_def <- Deflate_Than_Sum(consumption_so)
rm(consumption_so)

sum_consumption_we_def <- Deflate_Than_Sum(consumption_we)
rm(consumption_we)



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

Generate_Estimation_Data <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)),-6,-5))
  print(region)
  
  SUM_SPENT_DEF_REGION = as.name(paste0("SUM_SPENT_DEF_",region))
  RATE_TB_DEF_REGION = as.name(paste0("RATE_TB_DEF_",region))
  RATE_ST_DEF_REGION = as.name(paste0("RATE_ST_DEF_",region))
  RATE_INFL_REGION = as.name(paste0("RATE_INFL_",region))
  
  x %>%   
    complete(ISOWEEK,
             HOUSEHOLD_CODE) %>%
    group_by(HOUSEHOLD_CODE) %>%
    arrange(ISOWEEK) %>%
    mutate(Y = log(!!SUM_SPENT_DEF_REGION) - log(lag(!!SUM_SPENT_DEF_REGION, 
                                                     n = lag_in_weeks)),
           Z1 = lag(Y, n = 2)) %>%
    na.exclude() %>%
    left_join(rates_log_avg_ne,
              by = "ISOWEEK") %>%
    transmute(DATE = as.Date(ISOweek2date(paste(ISOWEEK, "1", sep = "-"))),
              Y = Y,
              X_TB = !!RATE_TB_DEF_REGION,
              X_ST = !!RATE_ST_DEF_REGION,
              Z1 = Z1,
              Z2_TB = lag(RATE_TB, n = 2), 
              Z2_ST = lag(RATE_ST, n = 2),
              Z3 = lag(!!RATE_INFL_REGION, n = 2)) %>% 
    na.exclude() %>% 
    ungroup() %>%
    rename(HOUSEHOLD = HOUSEHOLD_CODE) 

}

preliminary_estimator_ne <- Generate_Estimation_Data(sum_consumption_ne_def)
preliminary_estimator_mw <- Generate_Estimation_Data(sum_consumption_mw_def)
preliminary_estimator_so <- Generate_Estimation_Data(sum_consumption_so_def)
preliminary_estimator_we <- Generate_Estimation_Data(sum_consumption_we_def)

if (FALSE) {
  
  
  library(plm)
  cat("\014")
  zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
            data = preliminary_estimator_ne,
            model = "pooling",
            index = c("HOUSEHOLD", "DATE"))
  summary(zz)
  detach("package:plm", unload=TRUE)
  
  # Lance's Regression Code
  #plm(Y ~ LogR | YInst + Lag2LogNomR + Lag2Inf, data=Trips4_1, model='pooling', index=c('household_code', 'monthR'))
 
  # write_csv(preliminary_estimator_ne, "../data_1week_sample05_ne.csv")
}





