rm(list=ls())

library(tidyverse)
library(zoo)
library(reshape2)
library(ISOweek)
library(lubridate)
library(EnvStats)

base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
#base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code_new/deflate_then_deseason.R"))

lag_in_months = 1L

# For each week, take the average observed tbill/stock index
# and create a log rate over over "lag_in_months"
rates_log_avg <- 
  index_table %>%
  group_by(YEAR = year(DATE),
           MONTH = month(DATE)) %>% 
  summarise(AVG_INDEX_TB = mean(INDEX_TB),
            AVG_INDEX_ST = mean(INDEX_ST),
            AVG_INDEX_CPI_NE = mean(INDEX_CPI_NE),
            AVG_INDEX_CPI_MW = mean(INDEX_CPI_MW),
            AVG_INDEX_CPI_SO = mean(INDEX_CPI_SO),
            AVG_INDEX_CPI_WE = mean(INDEX_CPI_WE),
            AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_TB_DEF_MW = mean(INDEX_TB_DEF_MW),
            AVG_INDEX_TB_DEF_SO = mean(INDEX_TB_DEF_SO),
            AVG_INDEX_TB_DEF_WE = mean(INDEX_TB_DEF_WE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE),
            AVG_INDEX_ST_DEF_MW = mean(INDEX_ST_DEF_MW),
            AVG_INDEX_ST_DEF_SO = mean(INDEX_ST_DEF_SO),
            AVG_INDEX_ST_DEF_WE = mean(INDEX_ST_DEF_WE)) %>%
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
                      n = lag_in_months))
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

Monthly_Estimation_Data <- function(x) {
  
  # Extract region from dataframe name
  region <- str_to_upper(str_sub(deparse(substitute(x)), -2, -1))
  
  SUM_SPENT_DEF_REGION = as.name(paste0("SUM_SPENT_DEF_",region))
  RATE_TB_DEF_REGION = as.name(paste0("RATE_TB_DEF_",region))
  RATE_ST_DEF_REGION = as.name(paste0("RATE_ST_DEF_",region))
  RATE_INFL_REGION = as.name(paste0("RATE_INFL_",region))
  
  x %>%
    group_by(HOUSEHOLD_CODE,
             YEAR = year(PURCHASE_DATE),
             MONTH = month(PURCHASE_DATE)) %>% 
    summarise(!!SUM_SPENT_DEF_REGION := 
                sum(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>% 
    complete(YEAR,
             MONTH,
             HOUSEHOLD_CODE) %>%
    group_by(HOUSEHOLD_CODE) %>%
    arrange(YEAR, MONTH) %>%
    mutate(Y = log(!!SUM_SPENT_DEF_REGION) - log(lag(!!SUM_SPENT_DEF_REGION, 
                                                     n = lag_in_months)),
           Z1 = lag(Y, n = 2)) %>%
    na.exclude() %>%
    left_join(rates_log_avg,
              by = c("YEAR","MONTH")) %>%
    unite(YEAR_MONTH,
          YEAR:MONTH,
          sep = "-") %>%
    transmute(DATE = as.Date(paste(YEAR_MONTH, "1", sep = "-")),
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


estimation_data_1m <-
  bind_rows(Monthly_Estimation_Data(consumption_ds_def_ne),
            Monthly_Estimation_Data(consumption_ds_def_mw),
            Monthly_Estimation_Data(consumption_ds_def_so),
            Monthly_Estimation_Data(consumption_ds_def_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_1m,
          file.path(base_path, 
                    "csv_output/estimation_data_weekly_1m.csv"))

library(plm)
zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1m,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 1 Month")
print("======================================================")
summary(zz)
detach("package:plm", unload=TRUE)
rm(estimation_data_1m,
   zz,
   rates_log_avg)




if (FALSE) {
cat("\014")

  # write_csv(estimation_data, "../data_1month_sample05_ne.csv")
  
  # %>% 
  #  filter_all(any_vars(is.na(.)))
  
  #####################################################33
  ##############################################33333
  
  # Gathers inflation data and deflates consumption by region
  # Consumption data is too sparse, condense into monthly data
  Deflate_Than_Sum <- function(x) {
    
    # Extract region from dataframe name
    region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
    
    TOTAL_SPENT_DEF_REGION = 
      as.name(paste0("TOTAL_SPENT_DEF_", region))
    INDEX_CPI_REGION = 
      as.name(paste0("INDEX_CPI_", region))
    SUM_SPENT_DEF_REGION = 
      as.name(paste0("SUM_SPENT_DEF_", region))
    
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
               YEAR = year(PURCHASE_DATE),
               MONTH = month(PURCHASE_DATE)) %>% 
      summarise(!!SUM_SPENT_DEF_REGION := 
                  sum(!!TOTAL_SPENT_DEF_REGION)) %>%
      ungroup()
  }
  
  sum_consumption_ne_def <- 
    Deflate_Than_Sum(consumption_ne)
  rm(consumption_ne)
  
  sum_consumption_mw_def <- 
    Deflate_Than_Sum(consumption_mw)
  rm(consumption_mw)
  
  sum_consumption_so_def <- 
    Deflate_Than_Sum(consumption_so)
  rm(consumption_so)
  
  sum_consumption_we_def <- 
    Deflate_Than_Sum(consumption_we)
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
  
  
  Generate_Estimation_Data <- function(x) {
    
    # Extract region from dataframe name
    region <- str_to_upper(str_sub(deparse(substitute(x)),-6,-5))
    
    SUM_SPENT_DEF_REGION = as.name(paste0("SUM_SPENT_DEF_",region))
    RATE_TB_DEF_REGION = as.name(paste0("RATE_TB_DEF_",region))
    RATE_ST_DEF_REGION = as.name(paste0("RATE_ST_DEF_",region))
    RATE_INFL_REGION = as.name(paste0("RATE_INFL_",region))
    
    x %>%   
      complete(YEAR,
               MONTH,
               HOUSEHOLD_CODE) %>%
      group_by(HOUSEHOLD_CODE) %>%
      arrange(YEAR, MONTH) %>%
      mutate(Y = log(!!SUM_SPENT_DEF_REGION) - log(lag(!!SUM_SPENT_DEF_REGION, 
                                                       n = lag_in_months)),
             Z1 = lag(Y, n = 2)) %>%
      na.exclude() %>%
      left_join(rates_log_avg,
                by = c("YEAR","MONTH")) %>%
      unite(YEAR_MONTH,
            YEAR:MONTH,
            sep = "-") %>%
      transmute(DATE = as.Date(paste(YEAR_MONTH, "1", sep = "-")),
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
  
  
  estimation_data <-
    bind_rows(Generate_Estimation_Data(sum_consumption_ne_def),
              Generate_Estimation_Data(sum_consumption_mw_def),
              Generate_Estimation_Data(sum_consumption_so_def),
              Generate_Estimation_Data(sum_consumption_we_def)) %>% 
    arrange(HOUSEHOLD,DATE)
  
  rm(sum_consumption_ne_def,
     sum_consumption_mw_def,
     sum_consumption_so_def,
     sum_consumption_we_def)
  
  
  write_csv(estimation_data,
            file.path(base_path, 
                      "csv_output/estimation_data_monthly.csv"))
  
  library(plm)
  zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
            data = estimation_data,
            model = "pooling",
            index = c("HOUSEHOLD", "DATE"))
  
  print("Estimation for 1 Month")
  summary(zz)
  detach("package:plm", unload=TRUE)
  
  rm(estimation_data,zz)
  
  }