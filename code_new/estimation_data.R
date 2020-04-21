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

sum_ne <- 
  consumption_ne_def %>% 
  arrange(HOUSEHOLD_CODE,PURCHASE_DATE) %>%
  group_by(HOUSEHOLD_CODE, 
           WEEK = week(PURCHASE_DATE), 
           YEAR = year(PURCHASE_DATE)) %>% 
  summarise(SUM = sum(TOTAL_SPENT_DEF))


if (FALSE) {

test <- 
  consumption_ne_def %>% 
  arrange(HOUSEHOLD_CODE,PURCHASE_DATE) %>%
  group_by(HOUSEHOLD_CODE, 
           WEEK = week(PURCHASE_DATE), 
           YEAR = year(PURCHASE_DATE))



test2 <- 
  consumption_ne_def %>% 
  arrange(HOUSEHOLD_CODE,PURCHASE_DATE) %>% 
  group_by(HOUSEHOLD_CODE) %>% 
  mutate(SUM_SPENT_DEF = rollapply(TOTAL_SPENT_DEF,
                                   width = 7,
                                   FUN = sum,
                                   align = "left", 
                                   partial = TRUE))

}