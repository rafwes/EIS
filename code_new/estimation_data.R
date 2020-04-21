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
           ISOWEEK = ISOweek(PURCHASE_DATE)) %>% 
  summarise(SUM_SPENT_WEEK_DEF = sum(TOTAL_SPENT_DEF)) %>% 
  ungroup()

# For each week, take the average observed tbill/stock index
avg_indexes_ne <- 
  index_table %>% 
  select(DATE,INDEX_TB_DEF_NE,INDEX_ST_DEF_NE) %>% 
  group_by(ISOWEEK = ISOweek(DATE)) %>% 
  summarise(AVG_INDEX_TB_DEF_NE = mean(INDEX_TB_DEF_NE),
            AVG_INDEX_ST_DEF_NE = mean(INDEX_ST_DEF_NE)) %>% 
  ungroup()

# Transforms week/year into a single date (first day of the week)
# in order to perform lags calculations coherently
consumption_indexes_ne <- 
  sum_consumption_ne %>% 
  left_join(avg_indexes_ne,
            by = "ISOWEEK") %>%
  mutate(DATE = as.Date(ISOweek2date(paste(ISOWEEK,
                                           "1",
                                           sep = "-")))) %>%
  select(HOUSEHOLD_CODE,
         DATE,
         ISOWEEK,
         SUM_SPENT_WEEK_DEF,
         AVG_INDEX_TB_DEF_NE,
         AVG_INDEX_ST_DEF_NE) %>% 
  arrange(DATE) 




x <- paste(1999:2011, "-12-31", sep = "")
y <- as.Date(x)
data.frame(date = format(y), weekdate = date2ISOweek(y))
data.frame(date = x, weekdate = date2ISOweek(x))

data.frame(date = format(y), week = ISOweek(y))
a <- data.frame(date = x, week = ISOweek(x))


w <- paste("2009-W53", 1:7, sep = "-")
data.frame(weekdate = w, date = ISOweek2date(w))
# convert from calendar date to week date and back to calendar date
x <- paste(1999:2011, "-12-31", sep = "")
w <- date2ISOweek(x)
d <- ISOweek2date(w)
data.frame(date = x, weekdate = w, date2 = d)


x <- paste(1999:2011, "-12-31", sep = "")
y <- as.Date(x)
data.frame(date = format(y), weekday = ISOweekday(y))
data.frame(date = x, weekday = ISOweekday(x))


if (FALSE) {



}