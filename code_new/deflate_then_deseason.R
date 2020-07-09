# rm(list=ls())
# 
# library(dplyr)
# library(tidyr)
# library(tibble)
# library(readr)
# library(stringr)
# library(zoo)
# library(reshape2)
# library(ISOweek)
# library(lubridate)
# library(prophet)
# library(conflicted)
# #library(grid)
# #library(gridExtra)
# 
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
# conflict_prefer("as.Date", "base")
# conflict_prefer("as.Date.numeric", "base")
# 
# #base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
# base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

source(file.path(base_path,"EIS/code_new/interest_rates.R"))
source(file.path(base_path,"EIS/code_new/grocery_data.R"))


# Gathers inflation data and deflates consumption by region
DeflateConsumption <- function(x) {
  
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  
  x %>%
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

# Deflating consumption
consumption_def_ne <- 
  DeflateConsumption(consumption_ne)
rm(consumption_ne)

consumption_def_mw <- 
  DeflateConsumption(consumption_mw)
rm(consumption_mw)

consumption_def_so <- 
  DeflateConsumption(consumption_so)
rm(consumption_so)

consumption_def_we <- 
  DeflateConsumption(consumption_we)
rm(consumption_we)



#################
##### delete down
#################



# prophet needs single daily consumption value, we shall use mean consumption.
# save data parameters to reconstruct it later
perc_ne <- 
  consumption_def_ne %>% 
  group_by(PURCHASE_DATE) %>% 
  mutate(PERCENTAGE = TOTAL_SPENT_DEF / sum(TOTAL_SPENT_DEF),
         NUM_PANELISTS = n())

# setup data as expected by prophet, 
data_model_ne <- 
  consumption_def_ne %>% 
  group_by(PURCHASE_DATE) %>% 
  summarise(MEAN_SPENT_DAY = sum(TOTAL_SPENT_DEF) / n()) %>% 
  rename(ds = PURCHASE_DATE, 
         y = MEAN_SPENT_DAY)

# fit model and decompose by issuing predict
#model_ne <- prophet()
model_ne <- prophet(seasonality.mode = 'multiplicative')
model_ne <- fit.prophet(model_ne, data_model_ne)
data_decomposed_ne <- predict(model_ne)

# construct de-seasonal trip data from trend and residuals
consumption_ds_def_ne <- 
  data_decomposed_ne %>% 
  left_join(data_model_ne, by="ds") %>% 
  transmute(PURCHASE_DATE = as.Date(ds),
            DESEASONED = trend + y - yhat
            ) %>% 
  left_join(perc_ne, by="PURCHASE_DATE") %>% 
  mutate(TOTAL_SPENT_DS_DEF = PERCENTAGE * DESEASONED * NUM_PANELISTS) %>%
  filter(TOTAL_SPENT_DS_DEF > 0) %>% 
  select(PURCHASE_DATE,
         HOUSEHOLD_CODE,
         TOTAL_SPENT_DS_DEF)

rm(perc_ne,
   model_ne,
   data_model_ne,
   data_decomposed_ne,
   consumption_def_ne)

############################################

# prophet needs single daily consumption value
# save percentage of total spent for each household in a given day
perc_mw <- 
  consumption_def_mw %>% 
  group_by(PURCHASE_DATE) %>% 
  mutate(PERCENTAGE = TOTAL_SPENT_DEF / sum(TOTAL_SPENT_DEF),
         NUM_PANELISTS = n())

data_model_mw <- 
  consumption_def_mw %>% 
  group_by(PURCHASE_DATE) %>% 
  summarise(MEAN_SPENT_DAY = sum(TOTAL_SPENT_DEF) / n()) %>% 
  rename(ds = PURCHASE_DATE, 
         y = MEAN_SPENT_DAY)

# fit model and decompose by issuing predict
#model_mw <- prophet()
model_mw <- prophet(seasonality.mode = 'multiplicative')
model_mw <- fit.prophet(model_mw, data_model_mw)
data_decomposed_mw <- predict(model_mw)

# construct de-seasonal trip data from trend and residuals
consumption_ds_def_mw <- 
  data_decomposed_mw %>% 
  left_join(data_model_mw, by="ds") %>% 
  transmute(PURCHASE_DATE = as.Date(ds),
            DESEASONED = trend + y - yhat
  ) %>% 
  left_join(perc_mw, by="PURCHASE_DATE") %>% 
  mutate(TOTAL_SPENT_DS_DEF = PERCENTAGE * DESEASONED * NUM_PANELISTS) %>%
  filter(TOTAL_SPENT_DS_DEF > 0) %>% 
  select(PURCHASE_DATE,
         HOUSEHOLD_CODE,
         TOTAL_SPENT_DS_DEF)



rm(perc_mw,
   model_mw,
   data_model_mw,
   data_decomposed_mw,
   consumption_def_mw)

############################################

# prophet needs single daily consumption value
# save percentage of total spent for each household in a given day
perc_so <- 
  consumption_def_so %>% 
  group_by(PURCHASE_DATE) %>% 
  mutate(PERCENTAGE = TOTAL_SPENT_DEF / sum(TOTAL_SPENT_DEF),
         NUM_PANELISTS = n())

data_model_so <- 
  consumption_def_so %>% 
  group_by(PURCHASE_DATE) %>% 
  summarise(MEAN_SPENT_DAY = sum(TOTAL_SPENT_DEF) / n()) %>% 
  rename(ds = PURCHASE_DATE, 
         y = MEAN_SPENT_DAY)

# fit model and decompose by issuing predict
#model_so <- prophet()
model_so <- prophet(seasonality.mode = 'multiplicative')
model_so <- fit.prophet(model_so, data_model_so)
data_decomposed_so <- predict(model_so)

# construct de-seasonal trip data from trend and residuals
consumption_ds_def_so <- 
  data_decomposed_so %>% 
  left_join(data_model_so, by="ds") %>% 
  transmute(PURCHASE_DATE = as.Date(ds),
            DESEASONED = trend + y - yhat
  ) %>% 
  left_join(perc_so, by="PURCHASE_DATE") %>% 
  mutate(TOTAL_SPENT_DS_DEF = PERCENTAGE * DESEASONED * NUM_PANELISTS) %>%
  filter(TOTAL_SPENT_DS_DEF > 0) %>% 
  select(PURCHASE_DATE,
         HOUSEHOLD_CODE,
         TOTAL_SPENT_DS_DEF)

rm(perc_so,
   model_so,
   data_model_so,
   data_decomposed_so,
   consumption_def_so)


############################################

# prophet needs single daily consumption value
# save percentage of total spent for each household in a given day
perc_we <- 
  consumption_def_we %>% 
  group_by(PURCHASE_DATE) %>% 
  mutate(PERCENTAGE = TOTAL_SPENT_DEF / sum(TOTAL_SPENT_DEF),
         NUM_PANELISTS = n())

data_model_we <- 
  consumption_def_we %>% 
  group_by(PURCHASE_DATE) %>% 
  summarise(MEAN_SPENT_DAY = sum(TOTAL_SPENT_DEF) / n()) %>% 
  rename(ds = PURCHASE_DATE, 
         y = MEAN_SPENT_DAY)

# fit model and decompose by issuing predict
#model_we <- prophet()
model_we <- prophet(seasonality.mode = 'multiplicative')
model_we <- fit.prophet(model_we, data_model_we)
data_decomposed_we <- predict(model_we)

# construct de-seasonal trip data from trend and residuals
consumption_ds_def_we <- 
  data_decomposed_we %>% 
  left_join(data_model_we, by="ds") %>% 
  transmute(PURCHASE_DATE = as.Date(ds),
            DESEASONED = trend + y - yhat
  ) %>% 
  left_join(perc_we, by="PURCHASE_DATE") %>% 
  mutate(TOTAL_SPENT_DS_DEF = PERCENTAGE * DESEASONED * NUM_PANELISTS) %>%
  filter(TOTAL_SPENT_DS_DEF > 0) %>% 
  select(PURCHASE_DATE,
         HOUSEHOLD_CODE,
         TOTAL_SPENT_DS_DEF)

rm(perc_we,
   model_we,
   data_model_we,
   data_decomposed_we,
   consumption_def_we)



#prophet_plot_components(model, data_predicted_ne)

if (FALSE) {
  
plotdata <- aaa %>% 
  group_by(PURCHASE_DATE) %>% 
  summarize(SUM_TSD = mean(TOTAL_SPENT_DEF),
            SUM_TSDD = mean(TOTAL_SPENT_DS_DEF)
            ) %>% 
  ungroup() %>%
  transmute(DATE = PURCHASE_DATE,
            ROLL_TSD = scale(rollapply(SUM_TSD,
                                  3*28,
                                  mean,
                                  partial = TRUE,
                                  align = "center")),
            ROLL_TSDD = scale(rollapply(SUM_TSDD,
                                  3*28,
                                  mean,
                                  partial = TRUE,
                                  align = "center")),
            
            )


  

# y - s = t + (y - yhat)

library(ggplot2)
plot_1 <- 
ggplot() + 
  geom_line(data = plotdata %>%
              select(DATE,
                     ROLL_TSD,
                     ROLL_TSDD,
                     ) %>% 
              filter(between(DATE,
                             as.Date("2008-06-01"), 
                             as.Date("2010-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y"
               ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom")

plot_2 <- 
  ggplot() + 
  geom_line(data = plotdata %>%
              select(DATE,
                     ROLL_TSD,
                     ROLL_TSDD,
              ) %>% 
              filter(between(DATE,
                             as.Date("2010-06-01"), 
                             as.Date("2012-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y"
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom")

plot_3 <- 
  ggplot() + 
  geom_line(data = plotdata %>%
              select(DATE,
                     ROLL_TSD,
                     ROLL_TSDD,
              ) %>% 
              filter(between(DATE,
                             as.Date("2012-06-01"), 
                             as.Date("2014-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y"
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom")

plot_4 <- 
  ggplot() + 
  geom_line(data = plotdata %>%
              select(DATE,
                     ROLL_TSD,
                     ROLL_TSDD,
              ) %>% 
              filter(between(DATE,
                             as.Date("2014-06-01"), 
                             as.Date("2016-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y"
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(),
        legend.position = "bottom")


library(grid)
library(gridExtra)
grid.newpage()
grid.draw(rbind(ggplotGrob(plot_1), 
                ggplotGrob(plot_2),
                ggplotGrob(plot_3), 
                ggplotGrob(plot_4),
                size = "last"))


}

#################
##### delete up
#################


# plots reconstructed consumption data
if (FALSE) {
  
  plt_daily <-
    consumption_ds_def_ne %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG_TSD = mean(TOTAL_SPENT_DEF),
              AVG_TSDD = mean(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>% 
    mutate(DATE = PURCHASE_DATE,
           ROLL_TSD = rollapply(AVG_TSD,
                                3*28,
                                mean,
                                partial = TRUE,
                                align = "center"),
           ROLL_TSDD = rollapply(AVG_TSDD,
                                3*28,
                                mean,
                                partial = TRUE,
                                align = "center")
           )
  
  
  ggplot() + 
    geom_line(data = plt_daily %>%
                select(DATE,
                       ROLL_TSD,
                       ROLL_TSDD
                       ) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
}



# rm(DeflateConsumption,
#    SeasonalDummiesLM,
#    ConsumptionSeasonalityMatrix)



if (FALSE) {
  
  
  hist(consumption_ds_def_so$TOTAL_SPENT_DS_DEF,
       breaks=500,
       xlim = c(0,250))
  
  hist(consumption_ds_def_we$TOTAL_SPENT_DS_DEF,
       breaks=2000,
       xlim = c(0,30))
  
  
  daily_def_ne <-
    consumption_def_ne %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG = mean(TOTAL_SPENT_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG = scale(rollapply(AVG,
                                    3*28,
                                    mean,
                                    partial = TRUE,
                                    align = "center")))
  daily_ds_def_ne <-
    consumption_ds_def_ne %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG_DS_DEF = mean(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG_DS_DEF = scale(rollapply(AVG_DS_DEF,
                                           3*28,
                                           mean,
                                           partial = TRUE,
                                           align = "center")))
  plot_ne <- 
    ggplot() + 
    geom_line(data = daily_def_ne %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#00BFC4") +
    geom_line(data = daily_ds_def_ne %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#F8766D") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  daily_def_mw <-
    consumption_def_mw %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG = mean(TOTAL_SPENT_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG = scale(rollapply(AVG,
                                    3*28,
                                    mean,
                                    partial = TRUE,
                                    align = "center")))
  daily_ds_def_mw <-
    consumption_ds_def_mw %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG_DS_DEF = mean(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG_DS_DEF = scale(rollapply(AVG_DS_DEF,
                                           3*28,
                                           mean,
                                           partial = TRUE,
                                           align = "center")))
  plot_mw <- 
    ggplot() + 
    geom_line(data = daily_def_mw %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#00BFC4") +
    geom_line(data = daily_ds_def_mw %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#F8766D") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  daily_def_so <-
    consumption_def_so %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG = mean(TOTAL_SPENT_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG = scale(rollapply(AVG,
                                    3*28,
                                    mean,
                                    partial = TRUE,
                                    align = "center")))
  daily_ds_def_so <-
    consumption_ds_def_so %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG_DS_DEF = mean(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG_DS_DEF = scale(rollapply(AVG_DS_DEF,
                                           3*28,
                                           mean,
                                           partial = TRUE,
                                           align = "center")))
  plot_so <- 
    ggplot() + 
    geom_line(data = daily_def_so %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#00BFC4") +
    geom_line(data = daily_ds_def_so %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#F8766D") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  
  daily_def_we <-
    consumption_def_we %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG = mean(TOTAL_SPENT_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG = scale(rollapply(AVG,
                                    3*28,
                                    mean,
                                    partial = TRUE,
                                    align = "center")))
  daily_ds_def_we <-
    consumption_ds_def_we %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG_DS_DEF = mean(TOTAL_SPENT_DS_DEF)) %>%
    ungroup() %>% 
    transmute(DATE = PURCHASE_DATE,
              AVG_DS_DEF = scale(rollapply(AVG_DS_DEF,
                                           3*28,
                                           mean,
                                           partial = TRUE,
                                           align = "center")))
  plot_we <- 
    ggplot() + 
    geom_line(data = daily_def_we %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#00BFC4") +
    geom_line(data = daily_ds_def_we %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name),
              colour = "#F8766D") +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  rm(list=ls(pattern="^daily"))
  rm(list=ls(pattern="^consumption_def_"))
  
  
  if (FALSE) {   
    grid.newpage()
    grid.draw(rbind(ggplotGrob(plot_ne), 
                    ggplotGrob(plot_mw),
                    ggplotGrob(plot_so), 
                    ggplotGrob(plot_we),
                    size = "last"))
  }
  
  
  ggsave(file=file.path(base_path, 
                        "csv_output/deflate_then_deseason.pdf"), 
         arrangeGrob(plot_ne, 
                     plot_mw,
                     plot_so,
                     plot_we, 
                     nrow = 4), 
         width = 210, 
         height = 297, 
         units = "mm")
  
  
  rm(list=ls(pattern="^plot"))
  
}