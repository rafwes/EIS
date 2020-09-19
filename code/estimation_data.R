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
#base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code/financial_indexes.R"))
source(file.path(base_path,"EIS/code/consumption_data.R"))


panelists_selection <- 
  c(# "all",
    # "income_less25k",
    # "income_geq25k",
    # "income_geq50k",
    # "income_geq70k",
    # "age_malehead_leq34",
    # "age_malehead_geq35leq54",
    "age_malehead_geq55"
    # "age_femalehead_leq34",
    # "age_femalehead_geq35leq54",
    # "age_femalehead_geq55",
    # "college_both",
    # "college_one",
    # "college_none",
    # "ethnicity_white",
    # "ethnicity_black",
    # "ethnicity_asian",
    # "ethnicity_hispanic"
  )

# panelists_selection <- 
#   c("all",
#     "income_less25k"
#   )

# panelists_selection <- 
#   c("income_less25k")


# Trims dataset according to panelist characteristics
SelectPanelists <- function(df, selection) {

  if (selection == "all") {
    df_tmp <- df
  } 
  else if (selection == "income_less25k") {
    df_tmp <- df %>% 
      filter(HOUSEHOLD_INCOME < 15)
  }
  else if (selection == "income_geq25k") {
    df_tmp <- df %>% 
      filter(HOUSEHOLD_INCOME >= 15)
    }
  else if (selection == "income_geq50k") {
    df_tmp <- df %>% 
      filter(HOUSEHOLD_INCOME >= 21)
  }
  else if (selection == "income_geq70k") {
    df_tmp <- df %>% 
      filter(HOUSEHOLD_INCOME >= 26)
  }
  else if (selection == "age_malehead_leq34") {
    df_tmp <- df %>%
      filter(MALE_HEAD_AGE >= 1
             & MALE_HEAD_AGE < 4)
  }
  else if (selection == "age_malehead_geq35leq54") {
    df_tmp <- df %>% 
      filter(MALE_HEAD_AGE >= 4
             & MALE_HEAD_AGE < 8)
  }
  else if (selection == "age_malehead_geq55") {
    df_tmp <- df %>% 
      filter(MALE_HEAD_AGE == 8 
             | MALE_HEAD_AGE == 9)
  }
  else if (selection == "age_femalehead_leq34") {
    df_tmp <- df %>%
      filter(FEMALE_HEAD_AGE >= 1
             & FEMALE_HEAD_AGE < 4)
  }
  else if (selection == "age_femalehead_geq35leq54") {
    df_tmp <- df %>% 
      filter(FEMALE_HEAD_AGE >= 4
             & FEMALE_HEAD_AGE < 8)
  }
  else if (selection == "age_femalehead_geq55") {
    df_tmp <- df %>% 
      filter(FEMALE_HEAD_AGE == 8 
             | FEMALE_HEAD_AGE == 9)
  }
  else if (selection == "college_both") {
    df_tmp <- df %>% 
      filter(MALE_HEAD_EDUCATION >= 5 
             & FEMALE_HEAD_EDUCATION >= 5)
  }
  else if (selection == "college_one") {
    df_tmp <- df %>% 
      filter((MALE_HEAD_EDUCATION >= 5 
             & FEMALE_HEAD_EDUCATION < 5)
             | (MALE_HEAD_EDUCATION < 5 
                & FEMALE_HEAD_EDUCATION >= 5))
  }
  else if (selection == "college_none") {
    df_tmp <- df %>% 
      filter(MALE_HEAD_EDUCATION < 5 
             & FEMALE_HEAD_EDUCATION < 5)
  }
  else if (selection == "ethnicity_white") {
    df_tmp <- df %>% 
      filter(RACE == 1)
  }
  else if (selection == "ethnicity_black") {
    df_tmp <- df %>% 
      filter(RACE == 2)
  }
  else if (selection == "ethnicity_asian") {
    df_tmp <- df %>% 
      filter(RACE == 3)
  }
  else if (selection == "ethnicity_hispanic") {
    df_tmp <- df %>% 
      filter(HISPANIC_ORIGIN == 1)
  }
  else {print("Wrong selection")}
  
  df_final <-
    df_tmp %>%
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  #df_final <- df_tmp
  
  return(df_final)
    
}

#####################################################################
## DEFLATING AND DESEASONALIZING REGIONAL DAILY CONSUMPTION 
#####################################################################

# Gathers inflation data and deflates consumption by region
DeflateConsumption <- function(df, region) {
  
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  
  df %>%
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
DeseasonalizeConsumption <- function(df) {
  
  perc <- 
    df %>% 
    group_by(PURCHASE_DATE) %>% 
    mutate(PERCENTAGE = TOTAL_SPENT_DEF / sum(TOTAL_SPENT_DEF),
           NUM_PANELISTS = n())
  
  data_model <-
    df %>%
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

MatchRatesInstruments <- function(df, region) {
  
  # Extract region from dataframe name
  
  RATE_TB_DEF_REGION = as.name(paste0("RATE_TB_DEF_",region))
  RATE_INFL_REGION = as.name(paste0("RATE_INFL_",region))
  
  df %>%
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

for(var_selection in panelists_selection) {
  
  estimation_data_ne <- 
    consumption_ne %>% 
    SelectPanelists(var_selection) %>% 
    DeflateConsumption("NE") %>% 
    DeseasonalizeConsumption() %>% 
    MatchRatesInstruments("NE")

  estimation_data_mw <- 
    consumption_mw %>% 
    SelectPanelists(var_selection) %>% 
    DeflateConsumption("MW") %>% 
    DeseasonalizeConsumption() %>% 
    MatchRatesInstruments("MW")

  estimation_data_so <- 
    consumption_so %>% 
    SelectPanelists(var_selection) %>% 
    DeflateConsumption("SO") %>% 
    DeseasonalizeConsumption() %>% 
    MatchRatesInstruments("SO")

  estimation_data_we <- 
    consumption_we %>% 
    SelectPanelists(var_selection) %>% 
    DeflateConsumption("WE") %>% 
    DeseasonalizeConsumption() %>% 
    MatchRatesInstruments("WE")

  
  estimation_data <-
    bind_rows(estimation_data_ne,
              estimation_data_mw,
              estimation_data_so,
              estimation_data_we) %>% 
    arrange(HOUSEHOLD,DATE)
  
  filename <- 
    paste0("estimation_data_", 
           var_selection, 
           ".csv")
  
  write_csv(estimation_data,
            file.path(base_path, 
                      "csv_output",
                      "grocery_channel",
                      #"every_channel",
                      filename))
  
  zz_tb <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
            data = estimation_data,
            model = "pooling",
            index = c("HOUSEHOLD", "DATE"))
  
cat("==============================================================\n")
cat("OLS Estimation for '", var_selection, "' panelists \n")
cat("==============================================================\n")
print(summary(zz_tb))
}
