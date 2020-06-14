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
# Consumption data is too sparse, condense into weekly data
DeflateConsumption <- function(x) {
  
  region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
  INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
  
  x %>%
    left_join(index_table %>%
                select(DATE,!!INDEX_CPI_REGION),
              by = c("PURCHASE_DATE" = "DATE")
              ) %>% 
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

# averages daily consumption and creates a yearly moving average
daily_trips_def_ne <- 
  consumption_def_ne %>%
  group_by(PURCHASE_DATE) %>% 
  summarize(AVG = mean(TOTAL_SPENT_DEF)) %>% 
  ungroup() %>% 
  transmute(PURCHASE_DATE,
            AVG_ROLL = rollapply(AVG,
                                 366,
                                 mean,
                                 partial = TRUE,
                                 align = "center"))

daily_trips_def_mw <- 
  consumption_def_mw %>%
  group_by(PURCHASE_DATE) %>% 
  summarize(AVG = mean(TOTAL_SPENT_DEF)) %>% 
  ungroup() %>% 
  transmute(PURCHASE_DATE,
            AVG_ROLL = rollapply(AVG,
                                 366,
                                 mean,
                                 partial = TRUE,
                                 align = "center"))

daily_trips_def_so <- 
  consumption_def_so %>%
  group_by(PURCHASE_DATE) %>% 
  summarize(AVG = mean(TOTAL_SPENT_DEF)) %>% 
  ungroup() %>% 
  transmute(PURCHASE_DATE,
            AVG_ROLL = rollapply(AVG,
                                 366,
                                 mean,
                                 partial = TRUE,
                                 align = "center"))

daily_trips_def_we <- 
  consumption_def_we %>%
  group_by(PURCHASE_DATE) %>% 
  summarize(AVG = mean(TOTAL_SPENT_DEF)) %>% 
  ungroup() %>% 
  transmute(PURCHASE_DATE,
            AVG_ROLL = rollapply(AVG,
                                 366,
                                 mean,
                                 partial = TRUE,
                                 align = "center"))


# creates a zero-mean consumption in order to achieve 
# stationary series using 1-year rolling averages
consumption_zeromean_def_ne <- 
  consumption_def_ne %>% 
  left_join(daily_trips_def_ne,
            by = "PURCHASE_DATE") %>% 
  mutate(TSD_AR = 
           TOTAL_SPENT_DEF 
         - AVG_ROLL)

consumption_zeromean_def_mw <- 
  consumption_def_mw %>% 
  left_join(daily_trips_def_mw,
            by = "PURCHASE_DATE") %>% 
  mutate(TSD_AR = 
           TOTAL_SPENT_DEF 
         - AVG_ROLL)

consumption_zeromean_def_so <- 
  consumption_def_so %>% 
  left_join(daily_trips_def_so,
            by = "PURCHASE_DATE") %>% 
  mutate(TSD_AR = 
           TOTAL_SPENT_DEF 
         - AVG_ROLL)

consumption_zeromean_def_we <- 
  consumption_def_we %>% 
  left_join(daily_trips_def_we,
            by = "PURCHASE_DATE") %>% 
  mutate(TSD_AR = 
           TOTAL_SPENT_DEF 
         - AVG_ROLL)

rm(list=ls(pattern="^daily_trips"))
rm(list=ls(pattern="^consumption_def"))


# plots deflation of data
if (FALSE) {
  
  consumption_zeromean <- 
    consumption_def_we %>% 
    left_join(daily_trips_def_we,
              by = "PURCHASE_DATE") %>% 
    mutate(TSD = TOTAL_SPENT_DEF,
           TSD_AR = TOTAL_SPENT_DEF - AVG_ROLL)
  
  plt_daily <-
    consumption_zeromean %>%
    group_by(PURCHASE_DATE) %>% 
    summarize(AVG_TSD = mean(TSD),
              AVG_TSD_AR = mean(TSD_AR)) %>%
    ungroup() %>% 
    mutate(DATE = PURCHASE_DATE,
           ROLL_TSD = rollapply(AVG_TSD,
                                3*28,
                                mean,
                                partial = TRUE,
                                align = "center"),
           ROLL_TSD_AR = rollapply(AVG_TSD_AR,
                                   3*28,
                                   mean,
                                   partial = TRUE,
                                   align = "center")
    )
  
  
  ggplot() + 
    geom_line(data = plt_daily %>%
                select(DATE,
                       ROLL_TSD,
                       ROLL_TSD_AR) %>% 
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


# Setup dummy variable deseasonalization linear model
SeasonalDummiesLM <- function(x) {
  
  lm(Y ~ -1+
       W01+W02+W03+W04+W05+W06+W07+W08+W09+W10+
       W11+W12+W13+W14+W15+W16+W17+W18+W19+W20+
       W21+W22+W23+W24+W25+W26+W27+W28+W29+W30+
       W31+W32+W33+W34+W35+W36+W37+W38+W39+W40+
       W41+W42+W43+W44+W45+W46+W47+W48+W49+W50+
       W51+W52+W53,
     data = x)
}

# Append a dummy variable matrix for weekly deseasonalization
ConsumptionSeasonalityMatrix <- function(x) {
  
  x %>%
    arrange(HOUSEHOLD_CODE, PURCHASE_DATE) %>%
    transmute(Y = TSD_AR,
              W01 = case_when(isoweek(PURCHASE_DATE) == 1 ~ 1, TRUE ~ 0),
              W02 = case_when(isoweek(PURCHASE_DATE) == 2 ~ 1, TRUE ~ 0),
              W03 = case_when(isoweek(PURCHASE_DATE) == 3 ~ 1, TRUE ~ 0),
              W04 = case_when(isoweek(PURCHASE_DATE) == 4 ~ 1, TRUE ~ 0),
              W05 = case_when(isoweek(PURCHASE_DATE) == 5 ~ 1, TRUE ~ 0),
              W06 = case_when(isoweek(PURCHASE_DATE) == 6 ~ 1, TRUE ~ 0),
              W07 = case_when(isoweek(PURCHASE_DATE) == 7 ~ 1, TRUE ~ 0),
              W08 = case_when(isoweek(PURCHASE_DATE) == 8 ~ 1, TRUE ~ 0),
              W09 = case_when(isoweek(PURCHASE_DATE) == 9 ~ 1, TRUE ~ 0),
              W10 = case_when(isoweek(PURCHASE_DATE) == 10 ~ 1, TRUE ~ 0),
              W11 = case_when(isoweek(PURCHASE_DATE) == 11 ~ 1, TRUE ~ 0),
              W12 = case_when(isoweek(PURCHASE_DATE) == 12 ~ 1, TRUE ~ 0),
              W13 = case_when(isoweek(PURCHASE_DATE) == 13 ~ 1, TRUE ~ 0),
              W14 = case_when(isoweek(PURCHASE_DATE) == 14 ~ 1, TRUE ~ 0),
              W15 = case_when(isoweek(PURCHASE_DATE) == 15 ~ 1, TRUE ~ 0),
              W16 = case_when(isoweek(PURCHASE_DATE) == 16 ~ 1, TRUE ~ 0),
              W17 = case_when(isoweek(PURCHASE_DATE) == 17 ~ 1, TRUE ~ 0),
              W18 = case_when(isoweek(PURCHASE_DATE) == 18 ~ 1, TRUE ~ 0),
              W19 = case_when(isoweek(PURCHASE_DATE) == 19 ~ 1, TRUE ~ 0),
              W20 = case_when(isoweek(PURCHASE_DATE) == 20 ~ 1, TRUE ~ 0),
              W21 = case_when(isoweek(PURCHASE_DATE) == 21 ~ 1, TRUE ~ 0),
              W22 = case_when(isoweek(PURCHASE_DATE) == 22 ~ 1, TRUE ~ 0),
              W23 = case_when(isoweek(PURCHASE_DATE) == 23 ~ 1, TRUE ~ 0),
              W24 = case_when(isoweek(PURCHASE_DATE) == 24 ~ 1, TRUE ~ 0),
              W25 = case_when(isoweek(PURCHASE_DATE) == 25 ~ 1, TRUE ~ 0),
              W26 = case_when(isoweek(PURCHASE_DATE) == 26 ~ 1, TRUE ~ 0),
              W27 = case_when(isoweek(PURCHASE_DATE) == 27 ~ 1, TRUE ~ 0),
              W28 = case_when(isoweek(PURCHASE_DATE) == 28 ~ 1, TRUE ~ 0),
              W29 = case_when(isoweek(PURCHASE_DATE) == 29 ~ 1, TRUE ~ 0),
              W30 = case_when(isoweek(PURCHASE_DATE) == 30 ~ 1, TRUE ~ 0),
              W31 = case_when(isoweek(PURCHASE_DATE) == 31 ~ 1, TRUE ~ 0),
              W32 = case_when(isoweek(PURCHASE_DATE) == 32 ~ 1, TRUE ~ 0),
              W33 = case_when(isoweek(PURCHASE_DATE) == 33 ~ 1, TRUE ~ 0),
              W34 = case_when(isoweek(PURCHASE_DATE) == 34 ~ 1, TRUE ~ 0),
              W35 = case_when(isoweek(PURCHASE_DATE) == 35 ~ 1, TRUE ~ 0),
              W36 = case_when(isoweek(PURCHASE_DATE) == 36 ~ 1, TRUE ~ 0),
              W37 = case_when(isoweek(PURCHASE_DATE) == 37 ~ 1, TRUE ~ 0),
              W38 = case_when(isoweek(PURCHASE_DATE) == 38 ~ 1, TRUE ~ 0),
              W39 = case_when(isoweek(PURCHASE_DATE) == 39 ~ 1, TRUE ~ 0),
              W40 = case_when(isoweek(PURCHASE_DATE) == 40 ~ 1, TRUE ~ 0),
              W41 = case_when(isoweek(PURCHASE_DATE) == 41 ~ 1, TRUE ~ 0),
              W42 = case_when(isoweek(PURCHASE_DATE) == 42 ~ 1, TRUE ~ 0),
              W43 = case_when(isoweek(PURCHASE_DATE) == 43 ~ 1, TRUE ~ 0),
              W44 = case_when(isoweek(PURCHASE_DATE) == 44 ~ 1, TRUE ~ 0),
              W45 = case_when(isoweek(PURCHASE_DATE) == 45 ~ 1, TRUE ~ 0),
              W46 = case_when(isoweek(PURCHASE_DATE) == 46 ~ 1, TRUE ~ 0),
              W47 = case_when(isoweek(PURCHASE_DATE) == 47 ~ 1, TRUE ~ 0),
              W48 = case_when(isoweek(PURCHASE_DATE) == 48 ~ 1, TRUE ~ 0),
              W49 = case_when(isoweek(PURCHASE_DATE) == 49 ~ 1, TRUE ~ 0),
              W50 = case_when(isoweek(PURCHASE_DATE) == 50 ~ 1, TRUE ~ 0),
              W51 = case_when(isoweek(PURCHASE_DATE) == 51 ~ 1, TRUE ~ 0),
              W52 = case_when(isoweek(PURCHASE_DATE) == 52 ~ 1, TRUE ~ 0),
              W53 = case_when(isoweek(PURCHASE_DATE) == 53 ~ 1, TRUE ~ 0))
}

# Create deseasonalized consumption data and add rollmean back
# We need positive consumption data, therefore take max(coeff)

ds_model_ne <- 
  SeasonalDummiesLM(
    ConsumptionSeasonalityMatrix(
      consumption_zeromean_def_ne))

consumption_ds_def_ne <- 
  consumption_zeromean_def_ne %>%
  arrange(HOUSEHOLD_CODE, 
          PURCHASE_DATE) %>% 
  mutate(TOTAL_SPENT_DS_DEF = 
           residuals(ds_model_ne)
         + mean(coefficients(ds_model_ne))
         + AVG_ROLL
         ) %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DS_DEF)

#print(summary(ds_model_ne))
rm(consumption_zeromean_def_ne,
   ds_model_ne)



ds_model_mw <- 
  SeasonalDummiesLM(
    ConsumptionSeasonalityMatrix(
      consumption_zeromean_def_mw))

consumption_ds_def_mw <- 
  consumption_zeromean_def_mw %>%
  arrange(HOUSEHOLD_CODE, 
          PURCHASE_DATE) %>% 
  mutate(TOTAL_SPENT_DS_DEF = 
           residuals(ds_model_mw)
         + mean(coefficients(ds_model_mw))
         + AVG_ROLL
         ) %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DS_DEF)

#print(summary(ds_model_mw))
rm(consumption_zeromean_def_mw,
   ds_model_mw)


ds_model_so <- 
  SeasonalDummiesLM(
    ConsumptionSeasonalityMatrix(
      consumption_zeromean_def_so))

consumption_ds_def_so <- 
  consumption_zeromean_def_so %>%
  arrange(HOUSEHOLD_CODE, 
          PURCHASE_DATE) %>% 
  mutate(TOTAL_SPENT_DS_DEF = 
           residuals(ds_model_so)
         + mean(coefficients(ds_model_so))
         + AVG_ROLL
         ) %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DS_DEF)

#print(summary(ds_model_so))
rm(consumption_zeromean_def_so,
   ds_model_so)


ds_model_we <- 
  SeasonalDummiesLM(
    ConsumptionSeasonalityMatrix(
      consumption_zeromean_def_we))

consumption_ds_def_we <- 
  consumption_zeromean_def_we %>%
  arrange(HOUSEHOLD_CODE, 
          PURCHASE_DATE) %>% 
  mutate(TOTAL_SPENT_DS_DEF = 
           residuals(ds_model_we)
         + mean(coefficients(ds_model_we))
         + AVG_ROLL
         ) %>% 
  select(HOUSEHOLD_CODE,
         PURCHASE_DATE,
         TOTAL_SPENT_DS_DEF)

# print(summary(ds_model_we))
rm(consumption_zeromean_def_we,
   ds_model_we)


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



rm(DeflateConsumption,
   SeasonalDummiesLM,
   ConsumptionSeasonalityMatrix)



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

if (FALSE) {
  
  IndexSeasonalityMatrix <- function(x,y) {
    
    x %>%
      arrange(DATE) %>%
      transmute(Y = !!y,
                W01 = case_when(isoweek(DATE) == 1 ~ 1, TRUE ~ 0),
                W02 = case_when(isoweek(DATE) == 2 ~ 1, TRUE ~ 0),
                W03 = case_when(isoweek(DATE) == 3 ~ 1, TRUE ~ 0),
                W04 = case_when(isoweek(DATE) == 4 ~ 1, TRUE ~ 0),
                W05 = case_when(isoweek(DATE) == 5 ~ 1, TRUE ~ 0),
                W06 = case_when(isoweek(DATE) == 6 ~ 1, TRUE ~ 0),
                W07 = case_when(isoweek(DATE) == 7 ~ 1, TRUE ~ 0),
                W08 = case_when(isoweek(DATE) == 8 ~ 1, TRUE ~ 0),
                W09 = case_when(isoweek(DATE) == 9 ~ 1, TRUE ~ 0),
                W10 = case_when(isoweek(DATE) == 10 ~ 1, TRUE ~ 0),
                W11 = case_when(isoweek(DATE) == 11 ~ 1, TRUE ~ 0),
                W12 = case_when(isoweek(DATE) == 12 ~ 1, TRUE ~ 0),
                W13 = case_when(isoweek(DATE) == 13 ~ 1, TRUE ~ 0),
                W14 = case_when(isoweek(DATE) == 14 ~ 1, TRUE ~ 0),
                W15 = case_when(isoweek(DATE) == 15 ~ 1, TRUE ~ 0),
                W16 = case_when(isoweek(DATE) == 16 ~ 1, TRUE ~ 0),
                W17 = case_when(isoweek(DATE) == 17 ~ 1, TRUE ~ 0),
                W18 = case_when(isoweek(DATE) == 18 ~ 1, TRUE ~ 0),
                W19 = case_when(isoweek(DATE) == 19 ~ 1, TRUE ~ 0),
                W20 = case_when(isoweek(DATE) == 20 ~ 1, TRUE ~ 0),
                W21 = case_when(isoweek(DATE) == 21 ~ 1, TRUE ~ 0),
                W22 = case_when(isoweek(DATE) == 22 ~ 1, TRUE ~ 0),
                W23 = case_when(isoweek(DATE) == 23 ~ 1, TRUE ~ 0),
                W24 = case_when(isoweek(DATE) == 24 ~ 1, TRUE ~ 0),
                W25 = case_when(isoweek(DATE) == 25 ~ 1, TRUE ~ 0),
                W26 = case_when(isoweek(DATE) == 26 ~ 1, TRUE ~ 0),
                W27 = case_when(isoweek(DATE) == 27 ~ 1, TRUE ~ 0),
                W28 = case_when(isoweek(DATE) == 28 ~ 1, TRUE ~ 0),
                W29 = case_when(isoweek(DATE) == 29 ~ 1, TRUE ~ 0),
                W30 = case_when(isoweek(DATE) == 30 ~ 1, TRUE ~ 0),
                W31 = case_when(isoweek(DATE) == 31 ~ 1, TRUE ~ 0),
                W32 = case_when(isoweek(DATE) == 32 ~ 1, TRUE ~ 0),
                W33 = case_when(isoweek(DATE) == 33 ~ 1, TRUE ~ 0),
                W34 = case_when(isoweek(DATE) == 34 ~ 1, TRUE ~ 0),
                W35 = case_when(isoweek(DATE) == 35 ~ 1, TRUE ~ 0),
                W36 = case_when(isoweek(DATE) == 36 ~ 1, TRUE ~ 0),
                W37 = case_when(isoweek(DATE) == 37 ~ 1, TRUE ~ 0),
                W38 = case_when(isoweek(DATE) == 38 ~ 1, TRUE ~ 0),
                W39 = case_when(isoweek(DATE) == 39 ~ 1, TRUE ~ 0),
                W40 = case_when(isoweek(DATE) == 40 ~ 1, TRUE ~ 0),
                W41 = case_when(isoweek(DATE) == 41 ~ 1, TRUE ~ 0),
                W42 = case_when(isoweek(DATE) == 42 ~ 1, TRUE ~ 0),
                W43 = case_when(isoweek(DATE) == 43 ~ 1, TRUE ~ 0),
                W44 = case_when(isoweek(DATE) == 44 ~ 1, TRUE ~ 0),
                W45 = case_when(isoweek(DATE) == 45 ~ 1, TRUE ~ 0),
                W46 = case_when(isoweek(DATE) == 46 ~ 1, TRUE ~ 0),
                W47 = case_when(isoweek(DATE) == 47 ~ 1, TRUE ~ 0),
                W48 = case_when(isoweek(DATE) == 48 ~ 1, TRUE ~ 0),
                W49 = case_when(isoweek(DATE) == 49 ~ 1, TRUE ~ 0),
                W50 = case_when(isoweek(DATE) == 50 ~ 1, TRUE ~ 0),
                W51 = case_when(isoweek(DATE) == 51 ~ 1, TRUE ~ 0),
                W52 = case_when(isoweek(DATE) == 52 ~ 1, TRUE ~ 0),
                W53 = case_when(isoweek(DATE) == 53 ~ 1, TRUE ~ 0))
  }
  
  
  
}
