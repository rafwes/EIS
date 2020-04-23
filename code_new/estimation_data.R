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
# Y     = log(C_t) - log(C_{t-4})   ,where C_t is consumption for time t 
# X_TB  = log(1+r)                  ,where r is the real rate for t-bills
# X_ST  = log(1+r)                  ,same for stock returns
# Z1    = log(C_{t-1} - log{C_{t-5}}


crude_estimator_ne <- 
  sum_consumption_ne %>% 
  complete(ISOWEEK,HOUSEHOLD_CODE) %>%
  arrange(ISOWEEK) %>%
  group_by(HOUSEHOLD_CODE) %>% 
  mutate(Y = log(SUM_SPENT_WK_DEF_NE) - log(lag(SUM_SPENT_WK_DEF_NE, 
                                                n = lag_in_weeks)),
         Z1 = lag(Y, n = 1)) %>%
  na.exclude() %>%
  ungroup() %>% 
  left_join(avg_rates_ne,
            by = "ISOWEEK") %>%
  transmute(DATE = as.Date(ISOweek2date(paste(ISOWEEK, "1", sep = "-"))),
            HOUSEHOLD = HOUSEHOLD_CODE,
            Y = Y,
            X_TB = AVG_LOG_RATE_TB_DEF_NE,
            X_ST = AVG_LOG_RATE_ST_DEF_NE,
            Z1 = Z1)


if (FALSE) {
  
  library(plm)
  zz <- plm(Y ~ X_ST,
            data = crude_estimator_ne,
            model = "pooling",
            index = c("HOUSEHOLD", "DATE"))
  summary(zz)
  detach("package:plm", unload=TRUE)
  
  #plm(Y ~ LogR | YInst + Lag2LogNomR + Lag2Inf, data=Trips4_1, model='pooling', index=c('household_code', 'monthR'))
  
}





