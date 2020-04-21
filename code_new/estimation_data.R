rm(list=ls())

library(zoo)
library(lubridate)
library(ISOweek)

#base_path <- "/extra/agalvao/eis_nielsen"
base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code_new/interest_rates.R"))
source(file.path(base_path,"EIS/code_new/grocery_data.R"))

# Gathers inflation data and deflates consumption by region
consumption_ne_def <- 
  consumption_ne %>% 
  left_join(index_table %>% 
              select(DATE,INDEX_CPI_NE),
            by = c("PURCHASE_DATE" = "DATE")) %>% 
  mutate(TOTAL_SPENT_DEF_NE = 100 * TOTAL_SPENT / INDEX_CPI_NE) %>% 
  na.exclude() %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DEF_NE)

rm(consumption_ne)

# Consumption data is too sparse, condense into weekly data
sum_consumption_ne <- 
  consumption_ne_def %>% 
  arrange(HOUSEHOLD_CODE,PURCHASE_DATE) %>%
  group_by(HOUSEHOLD_CODE, 
           ISOWEEK = ISOweek(PURCHASE_DATE)) %>% 
  summarise(SUM_SPENT_WK_DEF_NE = sum(TOTAL_SPENT_DEF_NE)) %>%
  ungroup()


lag_in_weeks = 4L

# For each week, take the average observed tbill/stock index
# and create a log rate over 4 weeks
avg_rates_ne <- 
  index_table %>% 
  select(DATE,INDEX_TB_DEF_NE,INDEX_ST_DEF_NE) %>% 
  group_by(ISOWEEK = ISOweek(DATE)) %>% 
  summarise(AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE)) %>% 
  ungroup() %>% 
  mutate(AVG_LOG_RATE_TB_DEF_NE = 
           log(AVG_INDEX_TB_DEF_NE) - log(lag(AVG_INDEX_TB_DEF_NE,
                                              n = lag_in_weeks)),
         AVG_LOG_RATE_ST_DEF_NE = 
           log(AVG_INDEX_ST_DEF_NE) - log(lag(AVG_INDEX_ST_DEF_NE,
                                              n = lag_in_weeks))) %>% 
  select(ISOWEEK,
         AVG_LOG_RATE_TB_DEF_NE,
         AVG_LOG_RATE_ST_DEF_NE)

# Calculates lagged variables, drops observations for which no
# lags could be calculated and then joins them with rates and
# then delivers a proper date column since.
crude_estimator_ne <- 
  sum_consumption_ne %>% 
  complete(ISOWEEK,HOUSEHOLD_CODE) %>%
  arrange(ISOWEEK) %>%
  group_by(HOUSEHOLD_CODE) %>% 
  mutate(LOG_SPENT_GROWTH_DEF_NE = 
           log(SUM_SPENT_WK_DEF_NE) - log(lag(SUM_SPENT_WK_DEF_NE,
                                           n = lag_in_weeks))) %>%
  na.exclude() %>%
  ungroup() %>% 
  left_join(avg_rates_ne,
            by = "ISOWEEK") %>%
  mutate(DATE = as.Date(ISOweek2date(
                          paste(ISOWEEK, "1", sep = "-")))) %>% 
  select(DATE,
         HOUSEHOLD_CODE,
         LOG_SPENT_GROWTH_DEF_NE,
         AVG_LOG_RATE_TB_DEF_NE,
         AVG_LOG_RATE_ST_DEF_NE)


