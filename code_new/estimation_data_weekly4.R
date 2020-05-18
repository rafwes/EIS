  rm(list=ls())
  
  library(tidyverse)
  library(zoo)
  library(reshape2)
  library(ISOweek)
  library(lubridate)
  #library(EnvStats)
  
  #base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
  base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"
  
  source(file.path(base_path,"EIS/code_new/interest_rates.R"))
  source(file.path(base_path,"EIS/code_new/grocery_data.R"))
  
  lag_in_weeks = 4L
  
  
  # Gathers inflation data and deflates consumption by region
  # Consumption data is too sparse, condense into weekly data
  Deflate <- function(x) {
    
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
             !!TOTAL_SPENT_DEF_REGION)
  }

  
  consumption_def_ne <- 
    Deflate(consumption_ne)
  rm(consumption_ne)
  
  consumption_def_mw <- 
    Deflate(consumption_mw)
  rm(consumption_mw)
  
  consumption_def_so <- 
    Deflate(consumption_so)
  rm(consumption_so)
  
  consumption_def_we <- 
    Deflate(consumption_we)
  rm(consumption_we)
  
  ## Deseasonalization code
  
  # Append a dummy variable matrix for weekly deseasonalization
  Seasonality_Matrix <- function(x) {
    
    x %>%
      arrange(HOUSEHOLD_CODE, PURCHASE_DATE) %>%
      mutate(W01 = case_when(isoweek(PURCHASE_DATE) == 1 ~ 1,
                             TRUE ~ 0),
             W02 = case_when(isoweek(PURCHASE_DATE) == 2 ~ 1,
                             TRUE ~ 0),
             W03 = case_when(isoweek(PURCHASE_DATE) == 3 ~ 1,
                             TRUE ~ 0),
             W04 = case_when(isoweek(PURCHASE_DATE) == 4 ~ 1,
                             TRUE ~ 0),
             W05 = case_when(isoweek(PURCHASE_DATE) == 5 ~ 1,
                             TRUE ~ 0),
             W06 = case_when(isoweek(PURCHASE_DATE) == 6 ~ 1,
                             TRUE ~ 0),
             W07 = case_when(isoweek(PURCHASE_DATE) == 7 ~ 1,
                             TRUE ~ 0),
             W08 = case_when(isoweek(PURCHASE_DATE) == 8 ~ 1,
                             TRUE ~ 0),
             W09 = case_when(isoweek(PURCHASE_DATE) == 9 ~ 1,
                             TRUE ~ 0),
             W10 = case_when(isoweek(PURCHASE_DATE) == 10 ~ 1,
                             TRUE ~ 0),
             W11 = case_when(isoweek(PURCHASE_DATE) == 11 ~ 1,
                             TRUE ~ 0),
             W12 = case_when(isoweek(PURCHASE_DATE) == 12 ~ 1,
                             TRUE ~ 0),
             W13 = case_when(isoweek(PURCHASE_DATE) == 13 ~ 1,
                             TRUE ~ 0),
             W14 = case_when(isoweek(PURCHASE_DATE) == 14 ~ 1,
                             TRUE ~ 0),
             W15 = case_when(isoweek(PURCHASE_DATE) == 15 ~ 1,
                             TRUE ~ 0),
             W16 = case_when(isoweek(PURCHASE_DATE) == 16 ~ 1,
                             TRUE ~ 0),
             W17 = case_when(isoweek(PURCHASE_DATE) == 17 ~ 1,
                             TRUE ~ 0),
             W18 = case_when(isoweek(PURCHASE_DATE) == 18 ~ 1,
                             TRUE ~ 0),
             W19 = case_when(isoweek(PURCHASE_DATE) == 19 ~ 1,
                             TRUE ~ 0),
             W20 = case_when(isoweek(PURCHASE_DATE) == 20 ~ 1,
                             TRUE ~ 0),
             W21 = case_when(isoweek(PURCHASE_DATE) == 21 ~ 1,
                             TRUE ~ 0),
             W22 = case_when(isoweek(PURCHASE_DATE) == 22 ~ 1,
                             TRUE ~ 0),
             W23 = case_when(isoweek(PURCHASE_DATE) == 23 ~ 1,
                             TRUE ~ 0),
             W24 = case_when(isoweek(PURCHASE_DATE) == 24 ~ 1,
                             TRUE ~ 0),
             W25 = case_when(isoweek(PURCHASE_DATE) == 25 ~ 1,
                             TRUE ~ 0),
             W26 = case_when(isoweek(PURCHASE_DATE) == 26 ~ 1,
                             TRUE ~ 0),
             W27 = case_when(isoweek(PURCHASE_DATE) == 27 ~ 1,
                             TRUE ~ 0),
             W28 = case_when(isoweek(PURCHASE_DATE) == 28 ~ 1,
                             TRUE ~ 0),
             W29 = case_when(isoweek(PURCHASE_DATE) == 29 ~ 1,
                             TRUE ~ 0),
             W30 = case_when(isoweek(PURCHASE_DATE) == 30 ~ 1,
                             TRUE ~ 0),
             W31 = case_when(isoweek(PURCHASE_DATE) == 31 ~ 1,
                             TRUE ~ 0),
             W32 = case_when(isoweek(PURCHASE_DATE) == 32 ~ 1,
                             TRUE ~ 0),
             W33 = case_when(isoweek(PURCHASE_DATE) == 33 ~ 1,
                             TRUE ~ 0),
             W34 = case_when(isoweek(PURCHASE_DATE) == 34 ~ 1,
                             TRUE ~ 0),
             W35 = case_when(isoweek(PURCHASE_DATE) == 35 ~ 1,
                             TRUE ~ 0),
             W36 = case_when(isoweek(PURCHASE_DATE) == 36 ~ 1,
                             TRUE ~ 0),
             W37 = case_when(isoweek(PURCHASE_DATE) == 37 ~ 1,
                             TRUE ~ 0),
             W38 = case_when(isoweek(PURCHASE_DATE) == 38 ~ 1,
                             TRUE ~ 0),
             W39 = case_when(isoweek(PURCHASE_DATE) == 39 ~ 1,
                             TRUE ~ 0),
             W40 = case_when(isoweek(PURCHASE_DATE) == 40 ~ 1,
                             TRUE ~ 0),
             W41 = case_when(isoweek(PURCHASE_DATE) == 41 ~ 1,
                             TRUE ~ 0),
             W42 = case_when(isoweek(PURCHASE_DATE) == 42 ~ 1,
                             TRUE ~ 0),
             W43 = case_when(isoweek(PURCHASE_DATE) == 43 ~ 1,
                             TRUE ~ 0),
             W44 = case_when(isoweek(PURCHASE_DATE) == 44 ~ 1,
                             TRUE ~ 0),
             W45 = case_when(isoweek(PURCHASE_DATE) == 45 ~ 1,
                             TRUE ~ 0),
             W46 = case_when(isoweek(PURCHASE_DATE) == 46 ~ 1,
                             TRUE ~ 0),
             W47 = case_when(isoweek(PURCHASE_DATE) == 47 ~ 1,
                             TRUE ~ 0),
             W48 = case_when(isoweek(PURCHASE_DATE) == 48 ~ 1,
                             TRUE ~ 0),
             W49 = case_when(isoweek(PURCHASE_DATE) == 49 ~ 1,
                             TRUE ~ 0),
             W50 = case_when(isoweek(PURCHASE_DATE) == 50 ~ 1,
                             TRUE ~ 0),
             W51 = case_when(isoweek(PURCHASE_DATE) == 51 ~ 1,
                             TRUE ~ 0),
             W52 = case_when(isoweek(PURCHASE_DATE) == 52 ~ 1,
                             TRUE ~ 0),
             W53 = case_when(isoweek(PURCHASE_DATE) == 53 ~ 1,
                             TRUE ~ 0))
    
  }
  
  Deseasonlize <- function(x) {
    
    # Extract region from dataframe name
    region <- str_sub(deparse(substitute(x)),-2,-1)
    
    consumption_region = as.name(paste0("consumption_",region))
    
    lm(TOTAL_SPENT ~ -1+
         W01+W02+W03+W04+W05+W06+W07+W08+W09+W10+
         W11+W12+W13+W14+W15+W16+W17+W18+W19+W20+
         W21+W22+W23+W24+W25+W26+W27+W28+W29+W30+
         W31+W32+W33+W34+W35+W36+W37+W38+W39+W40+
         W41+W42+W43+W44+W45+W46+W47+W48+W49+W50+
         W51+W52+W53,
       data = Seasonality_Matrix(eval(consumption_region)))
  }
  
  
  model_ds_ne <- 
    Deseasonlize(consumption_ne)
  model_ds_mw <- 
    Deseasonlize(consumption_mw)
  model_ds_so <- 
    Deseasonlize(consumption_so)
  model_ds_we <- 
    Deseasonlize(consumption_we)
  
  # Creates deseasonlized consumption data
  # We need to reorder datapoints to match residual function
  consumption_ds_ne <- consumption_ne %>%
    arrange(HOUSEHOLD_CODE, PURCHASE_DATE) %>%
    select(HOUSEHOLD_CODE, PURCHASE_DATE) %>% 
    mutate(TOTAL_SPENT = residuals(model_ds_ne))
  
  rm(consumption_ne, model_ds_ne)
  
  consumption_ds_mw <- consumption_mw %>%
    arrange(HOUSEHOLD_CODE, PURCHASE_DATE) %>%
    select(HOUSEHOLD_CODE, PURCHASE_DATE) %>% 
    mutate(TOTAL_SPENT = residuals(model_ds_mw))
  
  rm(consumption_mw, model_ds_mw)
  
  consumption_ds_so <- consumption_so %>%
    arrange(HOUSEHOLD_CODE, PURCHASE_DATE) %>%
    select(HOUSEHOLD_CODE, PURCHASE_DATE) %>% 
    mutate(TOTAL_SPENT = residuals(model_ds_so))
  
  rm(consumption_so, model_ds_so)
  
  consumption_ds_we <- consumption_we %>%
    arrange(HOUSEHOLD_CODE, PURCHASE_DATE) %>%
    select(HOUSEHOLD_CODE, PURCHASE_DATE) %>% 
    mutate(TOTAL_SPENT = residuals(model_ds_we))
  
  rm(consumption_we, model_ds_we)
  
  
  
  
    
  

  
  
  
  # For each week, take the average observed tbill/stock index
  # and create a log rate over "lag_in_weeks"
  rates_log_avg <- 
    index_table %>%
    group_by(ISOWEEK = ISOweek(DATE)) %>% 
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
      left_join(rates_log_avg,
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
  
  
  estimation_data_4w <-
    bind_rows(Generate_Estimation_Data(sum_consumption_ne_def),
              Generate_Estimation_Data(sum_consumption_mw_def),
              Generate_Estimation_Data(sum_consumption_so_def),
              Generate_Estimation_Data(sum_consumption_we_def)) %>% 
    arrange(HOUSEHOLD,DATE)
  
  rm(sum_consumption_ne_def,
     sum_consumption_mw_def,
     sum_consumption_so_def,
     sum_consumption_we_def)
  
  
  write_csv(estimation_data_4w,
            file.path(base_path, 
                      "csv_output/estimation_data_weekly_4w.csv"))

  library(plm)
  zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
            data = estimation_data_4w,
            model = "pooling",
            index = c("HOUSEHOLD", "DATE"))
  
  print("Estimation for 4 Weeks") 
  summary(zz)
  detach("package:plm", unload=TRUE)
  
  rm(estimation_data_4w,zz)


if (FALSE) {
  cat("\014")
  
  # write_csv(estimation_data, "../data_1week_sample05_ne.csv")
  
  # %>% 
  #  filter_all(any_vars(is.na(.)))
  
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
  
   
}