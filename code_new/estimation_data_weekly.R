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

# Gathers inflation data and deflates consumption by region
# Consumption data is too sparse, condense into weekly data
sum_consumption_ne_def <- 
  consumption_ne %>% 
  left_join(index_table %>% 
              select(DATE,INDEX_CPI_NE),
            by = c("PURCHASE_DATE" = "DATE")) %>% 
  mutate(TOTAL_SPENT_DEF_NE = 100 * TOTAL_SPENT / INDEX_CPI_NE) %>% 
  na.exclude() %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DEF_NE) %>% 
  group_by(HOUSEHOLD_CODE, 
           ISOWEEK = ISOweek(PURCHASE_DATE)) %>% 
  summarise(SUM_SPENT_WK_DEF_NE = sum(TOTAL_SPENT_DEF_NE)) %>%
  ungroup()

#rm(consumption_ne)

lag_in_weeks = 4L

# For each week, take the average observed tbill/stock index
# and create a log rate over "lag_in_weeks" weeks
rates_log_avg_ne <- 
  index_table %>%
  group_by(ISOWEEK = ISOweek(DATE)) %>% 
  summarise(AVG_INDEX_CPI_NE = geoMean(INDEX_CPI_NE),
            AVG_INDEX_TB = geoMean(INDEX_TB),
            AVG_INDEX_ST = geoMean(INDEX_ST),
            AVG_INDEX_TB_DEF_NE = geoMean(INDEX_TB_DEF_NE),
            AVG_INDEX_ST_DEF_NE = geoMean(INDEX_ST_DEF_NE)) %>%
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
            RATE_TB_DEF_NE = 
              log(AVG_INDEX_TB_DEF_NE) 
            - log(lag(AVG_INDEX_TB_DEF_NE,
                      n = lag_in_weeks)),
            RATE_ST_DEF_NE = 
              log(AVG_INDEX_ST_DEF_NE) 
            - log(lag(AVG_INDEX_ST_DEF_NE,
                      n = lag_in_weeks))) %>%
  na.exclude()

# Calculates lagged variables, drops observations for which no
# lags could be calculated and then joins them with rates and
# then delivers a proper date column since.
# Y     = log(C_t) - log(C_{t-4})   ,where C_t is consumption for time t 
# X_TB  = log(1+r)                  ,where r is the real rate for t-bills
# X_ST  = log(1+r)                  ,same for stock returns
# Z1    = Y_{t-1} = log(C_{t-1} - log{C_{t-5}}
# Z2_TB = X_TB_{t-2} 
# Z2_ST = X_ST_{t-2}
# Z3    = log(1+\pi)_{t-2}          ,where \pi is the inflation rate
#
# Y calculation is prone to generate NA, therefore it's done earlier.

preliminary_estimator_ne <- 
  sum_consumption_ne_def %>% 
  complete(ISOWEEK,
           HOUSEHOLD_CODE) %>%
  group_by(HOUSEHOLD_CODE) %>%
  arrange(ISOWEEK) %>%
  mutate(Y = log(SUM_SPENT_WK_DEF_NE) - log(lag(SUM_SPENT_WK_DEF_NE, 
                                                n = lag_in_weeks)),
         Z1 = lag(Y, n = 2)) %>%
  na.exclude() %>%
  left_join(rates_log_avg_ne,
            by = "ISOWEEK") %>%
  transmute(DATE = as.Date(ISOweek2date(paste(ISOWEEK, "1", sep = "-"))),
            Y = Y,
            X_TB = RATE_TB_DEF_NE,
            X_ST = RATE_ST_DEF_NE,
            Z1 = Z1,
            Z2_TB = lag(RATE_TB, n = 2), 
            Z2_ST = lag(RATE_ST, n = 2),
            Z3 = lag(RATE_INFL_NE, n = 2)) %>% 
  na.exclude() %>% 
  ungroup() %>%
  rename(HOUSEHOLD = HOUSEHOLD_CODE)






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





