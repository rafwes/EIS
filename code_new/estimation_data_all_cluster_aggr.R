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
conflict_prefer("lead", "dplyr")
conflict_prefer("as.Date", "base")
conflict_prefer("as.Date.numeric", "base")
conflict_prefer("between", "dplyr")

base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
#base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code_new/interest_rates.R"))
source(file.path(base_path,"EIS/code_new/grocery_data.R"))

lag_in_months = 12L

index_table_monthly <- 
  index_table %>% 
  group_by(YEAR = year(DATE),
           MONTH = month(DATE)) %>% 
  summarise(across(starts_with("INDEX_"),
                   ~last(.))) %>%
  ungroup() %>% 
  mutate(across(starts_with("INDEX_"),
                ~100*./.[1]))

MonthlyEstimationData <- function(x) {
  
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))

x %>% 
  group_by(HOUSEHOLD_CODE,
           YEAR = year(PURCHASE_DATE),
           MONTH = month(PURCHASE_DATE)) %>% 
  summarise(SUM_SPENT = sum(TOTAL_SPENT)) %>% 
  ungroup() %>% 
  left_join(index_table_monthly,
            by = c("YEAR", "MONTH")) %>%
  transmute(HOUSEHOLD = HOUSEHOLD_CODE,
            YEAR,
            MONTH,
            SUM_SPENT_DEF := 100 * SUM_SPENT / !!INDEX_CPI_REGION,
            INDEX_TB,
            INDEX_ST,
            INDEX_CPI := !!INDEX_CPI_REGION,
            INDEX_TB_DEF := 100 * INDEX_TB / !!INDEX_CPI_REGION,
            INDEX_ST_DEF := 100 * INDEX_ST / !!INDEX_CPI_REGION) %>% 
  drop_na() %>% 
  group_by(HOUSEHOLD) %>% 
  complete(YEAR,
           MONTH,
           HOUSEHOLD) %>%
  arrange(YEAR, MONTH) %>%
  # turns consumption and indexes into log-rates relative to last period
  mutate(across(c(SUM_SPENT_DEF,
                  INDEX_TB,
                  INDEX_ST,
                  INDEX_CPI,
                  INDEX_TB_DEF,
                  INDEX_ST_DEF),
                ~ log(.) - log(lag(., n = lag_in_months)), )) %>%
  # twice lagged instruments
  mutate(across(c(SUM_SPENT_DEF,
                  INDEX_TB,
                  INDEX_ST,
                  INDEX_CPI,
                  INDEX_TB_DEF,
                  INDEX_ST_DEF),
                ~ lag(., n = 2),
                .names = "LAG_{col}" )) %>% 
  rename_with(~str_replace(., "SUM_SPENT_DEF", "GROWTH_C")) %>%
  rename_with(~str_replace(., "INDEX", "RATE")) %>%
  ungroup() %>% 
  drop_na() %>% 
  arrange(HOUSEHOLD, YEAR, MONTH) %>%
  # very few panelists change regions in a given year (ugly hack)
  mutate(DAY = case_when(region == "NE" ~ 1,
                         region == "MW" ~ 2,
                         region == "SO" ~ 3,
                         region == "WE" ~ 4)) %>% 
  unite(CHAR_DATE,YEAR,MONTH,DAY,sep = "-") %>%
  transmute(DATE = as.Date(CHAR_DATE),
            HOUSEHOLD,
            Y = GROWTH_C,
            #RATE_TB,
            #RATE_ST,
            #RATE_CPI,
            X_TB = RATE_TB_DEF,
            X_ST = RATE_ST_DEF,
            Z1 = LAG_GROWTH_C,
            Z2_TB = LAG_RATE_TB,
            Z2_ST = LAG_RATE_ST,
            Z3 = LAG_RATE_CPI,
            #LAG_RATE_TB_DEF,
            #LAG_RATE_ST_DEF,
            )

}

estimation_data_1m <- 
  bind_rows(MonthlyEstimationData(consumption_ne),
            MonthlyEstimationData(consumption_mw),
            MonthlyEstimationData(consumption_so),
            MonthlyEstimationData(consumption_we)) %>%
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
rm(estimation_data_1m, zz, index_table_monthly)

###############################################################################################


lag_in_quarters = 4L

index_table_quarterly <- 
  index_table %>% 
  group_by(YEAR = year(DATE),
           QUARTER = quarter(DATE)) %>% 
  summarise(across(starts_with("INDEX_"),
                   ~last(.))) %>%
  ungroup() %>% 
  mutate(across(starts_with("INDEX_"),
                ~100*./.[1]))

QuarterlyEstimationData <- function(x) {
  
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  
  x %>% 
    group_by(HOUSEHOLD_CODE,
             YEAR = year(PURCHASE_DATE),
             QUARTER = quarter(PURCHASE_DATE)) %>% 
    summarise(SUM_SPENT = sum(TOTAL_SPENT)) %>% 
    ungroup() %>% 
    left_join(index_table_quarterly,
              by = c("YEAR", "QUARTER")) %>%
    transmute(HOUSEHOLD = HOUSEHOLD_CODE,
              YEAR,
              QUARTER,
              SUM_SPENT_DEF := 100 * SUM_SPENT / !!INDEX_CPI_REGION,
              INDEX_TB,
              INDEX_ST,
              INDEX_CPI := !!INDEX_CPI_REGION,
              INDEX_TB_DEF := 100 * INDEX_TB / !!INDEX_CPI_REGION,
              INDEX_ST_DEF := 100 * INDEX_ST / !!INDEX_CPI_REGION) %>% 
    drop_na() %>% 
    group_by(HOUSEHOLD) %>% 
    complete(YEAR,
             QUARTER,
             HOUSEHOLD) %>%
    arrange(YEAR, QUARTER) %>%
    # turns consumption and indexes into log-rates relative to last period
    mutate(across(c(SUM_SPENT_DEF,
                    INDEX_TB,
                    INDEX_ST,
                    INDEX_CPI,
                    INDEX_TB_DEF,
                    INDEX_ST_DEF),
                  ~ log(.) - log(lag(., n = lag_in_quarters)), )) %>%
    # twice lagged instruments
    mutate(across(c(SUM_SPENT_DEF,
                    INDEX_TB,
                    INDEX_ST,
                    INDEX_CPI,
                    INDEX_TB_DEF,
                    INDEX_ST_DEF),
                  ~ lag(., n = 2),
                  .names = "LAG_{col}" )) %>% 
    rename_with(~str_replace(., "SUM_SPENT_DEF", "GROWTH_C")) %>%
    rename_with(~str_replace(., "INDEX", "RATE")) %>%
    ungroup() %>% 
    drop_na() %>% 
    arrange(HOUSEHOLD, YEAR, QUARTER) %>%
    # very few panelists change regions in a given year (ugly hack)
    mutate(MONTH = case_when(QUARTER == 1 ~ 1,
                             QUARTER == 2 ~ 4,
                             QUARTER == 3 ~ 7,
                             QUARTER == 4 ~ 10),
           DAY = case_when(region == "NE" ~ 1,
                           region == "MW" ~ 2,
                           region == "SO" ~ 3,
                           region == "WE" ~ 4)) %>%
    unite(CHAR_DATE,YEAR,MONTH,DAY,sep = "-") %>%
    transmute(DATE = as.Date(CHAR_DATE),
              HOUSEHOLD,
              Y = GROWTH_C,
              #RATE_TB,
              #RATE_ST,
              #RATE_CPI,
              X_TB = RATE_TB_DEF,
              X_ST = RATE_ST_DEF,
              Z1 = LAG_GROWTH_C,
              Z2_TB = LAG_RATE_TB,
              Z2_ST = LAG_RATE_ST,
              Z3 = LAG_RATE_CPI,
              #LAG_RATE_TB_DEF,
              #LAG_RATE_ST_DEF,
    )
  
}

estimation_data_1q <-
  bind_rows(QuarterlyEstimationData(consumption_ne),
            QuarterlyEstimationData(consumption_mw),
            QuarterlyEstimationData(consumption_so),
            QuarterlyEstimationData(consumption_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_1q,
          file.path(base_path, 
                    "csv_output/estimation_data_quarterly_1q.csv"))

zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1q,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 1 Quarter")
print("======================================================")
print(summary(zz))
rm(estimation_data_1q, zz, index_table_quarterly)




###############################################################################################

lag_in_years = 1L

index_table_yearly <- 
  index_table %>% 
  group_by(YEAR = year(DATE)) %>% 
  summarise(across(starts_with("INDEX_"),
                   ~last(.))) %>%
  ungroup() %>% 
  mutate(across(starts_with("INDEX_"),
                ~100*./.[1]))

YearlyEstimationData <- function(x) {
  
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  
  x %>% 
    group_by(HOUSEHOLD_CODE,
             YEAR = year(PURCHASE_DATE)) %>% 
    summarise(SUM_SPENT = sum(TOTAL_SPENT)) %>% 
    ungroup() %>% 
    left_join(index_table_yearly,
              by = "YEAR") %>%
    transmute(HOUSEHOLD = HOUSEHOLD_CODE,
              YEAR,
              SUM_SPENT_DEF := 100 * SUM_SPENT / !!INDEX_CPI_REGION,
              INDEX_TB,
              INDEX_ST,
              INDEX_CPI := !!INDEX_CPI_REGION,
              INDEX_TB_DEF := 100 * INDEX_TB / !!INDEX_CPI_REGION,
              INDEX_ST_DEF := 100 * INDEX_ST / !!INDEX_CPI_REGION) %>% 
    drop_na() %>% 
    group_by(HOUSEHOLD) %>% 
    complete(YEAR,
             HOUSEHOLD) %>%
    arrange(YEAR) %>%
    # turns consumption and indexes into log-rates relative to last period
    mutate(across(c(SUM_SPENT_DEF,
                    INDEX_TB,
                    INDEX_ST,
                    INDEX_CPI,
                    INDEX_TB_DEF,
                    INDEX_ST_DEF),
                  ~ log(.) - log(lag(., n = lag_in_years)), )) %>%
    # twice lagged instruments
    mutate(across(c(SUM_SPENT_DEF,
                    INDEX_TB,
                    INDEX_ST,
                    INDEX_CPI,
                    INDEX_TB_DEF,
                    INDEX_ST_DEF),
                  ~ lag(., n = 2),
                  .names = "LAG_{col}" )) %>% 
    rename_with(~str_replace(., "SUM_SPENT_DEF", "GROWTH_C")) %>%
    rename_with(~str_replace(., "INDEX", "RATE")) %>%
    ungroup() %>% 
    drop_na() %>% 
    arrange(HOUSEHOLD, YEAR) %>%
    # very few panelists change regions in a given year (ugly hack)
    mutate(MONTH = 1,
           DAY = case_when(region == "NE" ~ 1,
                           region == "MW" ~ 2,
                           region == "SO" ~ 3,
                           region == "WE" ~ 4)) %>%
    unite(CHAR_DATE,YEAR,MONTH,DAY,sep = "-") %>%
    transmute(DATE = as.Date(CHAR_DATE),
              HOUSEHOLD,
              Y = GROWTH_C,
              #RATE_TB,
              #RATE_ST,
              #RATE_CPI,
              X_TB = RATE_TB_DEF,
              X_ST = RATE_ST_DEF,
              Z1 = LAG_GROWTH_C,
              Z2_TB = LAG_RATE_TB,
              Z2_ST = LAG_RATE_ST,
              Z3 = LAG_RATE_CPI,
              #LAG_RATE_TB_DEF,
              #LAG_RATE_ST_DEF,
    )
  
}


estimation_data_1y <-
  bind_rows(YearlyEstimationData(consumption_ne),
            YearlyEstimationData(consumption_mw),
            YearlyEstimationData(consumption_so),
            YearlyEstimationData(consumption_we)) %>% 
  arrange(HOUSEHOLD,DATE)

write_csv(estimation_data_1y,
          file.path(base_path, 
                    "csv_output/estimation_data_yearly_1y.csv"))

zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
          data = estimation_data_1y,
          model = "pooling",
          index = c("HOUSEHOLD", "DATE"))

print("Estimation for 1 Year")
print("======================================================")
print(summary(zz))
rm(estimation_data_1y, zz, index_table_yearly)



################################
### garbage bin
################################


if (FALSE) {
  
 a <- data.frame(table(index(pdata.frame(estimation_data_1y, index = c("HOUSEHOLD", "DATE"))), useNA = "ifany"))
 b <- estimation_data_1y %>% filter(day(DATE) > 1)

 }