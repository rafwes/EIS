rm(list=ls())

library(zoo)
library(lubridate)

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
  mutate(TOTAL_SPENT_DEF = 100 * TOTAL_SPENT / INDEX_CPI_NE) %>% 
  na.exclude() %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DEF)

rm(consumption_ne)


# Consumption data is too sparse, condense into weekly data
sum_consumption_ne <- 
  consumption_ne_def %>% 
  arrange(HOUSEHOLD_CODE,PURCHASE_DATE) %>%
  group_by(HOUSEHOLD_CODE, 
           WEEK = week(PURCHASE_DATE), 
           YEAR = year(PURCHASE_DATE)) %>% 
  summarise(SUM_SPENT_WEEK_DEF = sum(TOTAL_SPENT_DEF))

# For each week, take the average observed tbill/stock index
avg_indexes_ne <- 
  index_table %>% 
  select(DATE,INDEX_TB_DEF_NE,INDEX_ST_DEF_NE) %>% 
  group_by(WEEK = week(DATE), 
           YEAR = year(DATE)) %>% 
  summarise(AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE))

joint_data <- 
  sum_consumption_ne %>% 
  left_join(avg_indexes_ne,
            by = c("WEEK", "YEAR"))







if (FALSE) {



}