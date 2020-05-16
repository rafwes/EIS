  rm(list=ls())
  
  library(tidyverse)
  library(zoo)
  library(reshape2)
  #library(ISOweek)
  library(lubridate)
  #library(EnvStats)
  library(grid)
  #library(forecast)
  #library(xts)
  
  
  #base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
  base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"
  
  source(file.path(base_path,"EIS/code_new/interest_rates.R"))
  source(file.path(base_path,"EIS/code_new/grocery_data.R"))
  
  
  # Gathers inflation data and deflates consumption by region
  # Consumption data is too sparse, condense into weekly data
  Deflate_Than_Sum <- function(x) {
    
    # Extract region from dataframe name
    region <- str_to_upper(str_sub(deparse(substitute(x)),-2,-1))
    
    AVG_TRIP_3M_REGION = as.name(paste0("AVG_TRIP_3M_",region))
    AVG_TRIP_7D_REGION = as.name(paste0("AVG_TRIP_7D_",region))
    INDEX_CPI_REGION = as.name(paste0("INDEX_CPI_",region))
    TREND_1Y_REGION = as.name(paste0("TREND_1Y_",region))
    
    x %>%
      group_by(PURCHASE_DATE) %>% 
      summarize(AVG_TRIP = mean(TOTAL_SPENT)) %>%
      ungroup() %>% 
      left_join(index_table %>%
                  select(DATE, !!INDEX_CPI_REGION),
                by = c("PURCHASE_DATE" = "DATE")) %>% 
      mutate(AVG_TRIP := 
               100 * AVG_TRIP 
             / !!INDEX_CPI_REGION) %>% 
      na.exclude() %>% 
      transmute(DATE = PURCHASE_DATE,
                !!AVG_TRIP_7D_REGION := 
                  rollapply(AVG_TRIP,
                            8,
                            mean,
                            partial = TRUE,
                            align = "center"),
                !!AVG_TRIP_3M_REGION := 
                  rollapply(AVG_TRIP,
                            3*28,
                            mean,
                            partial = TRUE,
                            align = "center"),
                !!TREND_1Y_REGION :=
                  rollapply(AVG_TRIP,
                            366,
                            mean,
                            partial = TRUE,
                            align = "center"))
  }
  
  
  sum_consumption_def_ne <- 
    Deflate_Than_Sum(consumption_ne)
  sum_consumption_ds_def_ne <- 
    Deflate_Than_Sum(consumption_ds_ne)

  sum_consumption_def_mw <- 
    Deflate_Than_Sum(consumption_mw)
  sum_consumption_ds_def_mw <- 
    Deflate_Than_Sum(consumption_ds_mw)

  sum_consumption_def_so <- 
    Deflate_Than_Sum(consumption_so)
  sum_consumption_ds_def_so <- 
    Deflate_Than_Sum(consumption_ds_so)
  
  sum_consumption_def_we <- 
    Deflate_Than_Sum(consumption_we)
  sum_consumption_ds_def_we <- 
    Deflate_Than_Sum(consumption_ds_we)
  
  
  plot_ne <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_ne %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_NE = scale(AVG_TRIP_3M_NE)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    geom_line(data = sum_consumption_ds_def_ne %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_DS_NE = scale(AVG_TRIP_3M_NE)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name))+
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")

  plot_mw <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_mw %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_MW = scale(AVG_TRIP_3M_MW)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    geom_line(data = sum_consumption_ds_def_mw %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_DS_MW = scale(AVG_TRIP_3M_MW)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name))+
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  plot_so <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_so %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_SO = scale(AVG_TRIP_3M_SO)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    geom_line(data = sum_consumption_ds_def_so %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_DS_SO = scale(AVG_TRIP_3M_SO)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name))+
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  plot_we <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_we %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_WE = scale(AVG_TRIP_3M_WE)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    geom_line(data = sum_consumption_ds_def_we %>%
                transmute(DATE = DATE,
                          AVG_TRIP_3M_DS_WE = scale(AVG_TRIP_3M_WE)) %>%
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name))+
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_text(),
          legend.position = "bottom")
  
  
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(plot_ne),
                  ggplotGrob(plot_mw),
                  ggplotGrob(plot_so),
                  ggplotGrob(plot_we),
                  size = "last"))
  
  
  ## Last week code ##
  #############################################
  
  
  sum_consumption_ne_def <- 
    Deflate_Than_Sum(consumption_ne)
 
  sum_consumption_mw_def <- 
    Deflate_Than_Sum(consumption_mw)
  
  sum_consumption_so_def <- 
    Deflate_Than_Sum(consumption_so)
  
  sum_consumption_we_def <- 
    Deflate_Than_Sum(consumption_we)
 
plot_ne <- 
  ggplot() + 
   geom_line(data = sum_consumption_ne_def %>% 
               filter(between(DATE,
                              as.Date("2012-06-01"), 
                              as.Date("2014-06-01"))) %>% 
               pivot_longer(-DATE),
             aes(x = DATE, 
                 y = value, 
                 colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_mw <- 
  ggplot() + 
  geom_line(data = sum_consumption_mw_def %>% 
              filter(between(DATE,
                             as.Date("2012-06-01"), 
                             as.Date("2014-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_so <- 
  ggplot() + 
  geom_line(data = sum_consumption_so_def %>% 
              filter(between(DATE,
                             as.Date("2012-06-01"), 
                             as.Date("2014-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())

plot_we <- 
  ggplot() + 
  geom_line(data = sum_consumption_we_def %>% 
              filter(between(DATE,
                             as.Date("2012-06-01"), 
                             as.Date("2014-06-01"))) %>% 
              pivot_longer(-DATE),
            aes(x = DATE, 
                y = value, 
                colour = name)) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text())


grid.newpage()
grid.draw(rbind(ggplotGrob(plot_ne), 
                ggplotGrob(plot_mw),
                ggplotGrob(plot_so), 
                ggplotGrob(plot_we),
                size = "last"))
   
  
  # Calculates lagged variables, drops observations for which no
  # lags could be calculated and then joins them with rates and
  # then delivers a proper date column since.
  # Y     = log(C_t) - log(C_{t-4})   ,where C_t is consumption for time t 
  # X_TB  = log(1+r)                  ,where r is the real rate for t-bills
  # X_ST  = log(1+r)                  ,same for stock returns
  # Z1    = Y_{t-2} = log(C_{t-2} - log{C_{t-6}}
  # Z2_TB = X_TB_{t-2} 
  # Z2_ST = X_ST_{t-2}
  # Z3    = log(1+\pi)_{t-2}          ,where \pi is the inflation rate
  #
  # Y calculation is prone to generate NA, therefore it's done earlier.
  
  Generate_Estimation_Data <- function(x) {
    
    # Extract region from dataframe name
    region <- str_to_upper(str_sub(deparse(substitute(x)),-6,-5))
    
    SUM_SPENT_DEF_REGION = as.name(paste0("SUM_SPENT_DEF_",region))
    RATE_TB_DEF_REGION = as.name(paste0("RATE_TB_DEF_",region))
    RATE_ST_DEF_REGION = as.name(paste0("RATE_ST_DEF_",region))
    RATE_INFL_REGION = as.name(paste0("RATE_INFL_",region))
    
    x %>%   
      complete(ISOWEEK,
               HOUSEHOLD_CODE) %>%
      group_by(HOUSEHOLD_CODE) %>%
      arrange(ISOWEEK) %>%
      mutate(Y = log(!!SUM_SPENT_DEF_REGION) - log(lag(!!SUM_SPENT_DEF_REGION, 
                                                       n = lag_in_days)),
             Z1 = lag(Y, n = 2)) %>%
      na.exclude() %>%
      left_join(rates_log,
                by = "ISOWEEK") %>%
      transmute(DATE = as.Date(ISOweek2date(paste(ISOWEEK, "1", sep = "-"))),
                Y = Y,
                X_TB = !!RATE_TB_DEF_REGION,
                X_ST = !!RATE_ST_DEF_REGION,
                Z1 = Z1,
                Z2_TB = lag(RATE_TB, n = 2), 
                Z2_ST = lag(RATE_ST, n = 2),
                Z3 = lag(!!RATE_INFL_REGION, n = 2)) %>% 
      na.exclude() %>% 
      ungroup() %>%
      rename(HOUSEHOLD = HOUSEHOLD_CODE) 
  
  }
  
  
  estimation_data_4w <-
    bind_rows(Generate_Estimation_Data(sum_consumption_ne_def),
              Generate_Estimation_Data(sum_consumption_mw_def),
              Generate_Estimation_Data(sum_consumption_so_def),
              Generate_Estimation_Data(sum_consumption_we_def)) %>% 
    arrange(HOUSEHOLD,DATE)
  
  rm(sum_consumption_ne_def,
     sum_consumption_mw_def,
     sum_consumption_so_def,
     sum_consumption_we_def)
  
  
  write_csv(estimation_data_4w,
            file.path(base_path, 
                      "csv_output/estimation_data_weekly_4w.csv"))

  library(plm)
  zz <- plm(Y ~ X_TB | Z1 + Z2_TB + Z3,
            data = estimation_data_4w,
            model = "pooling",
            index = c("HOUSEHOLD", "DATE"))
  
  print("Estimation for 4 Weeks") 
  summary(zz)
  detach("package:plm", unload=TRUE)
  
  rm(estimation_data_4w,zz)
  


##########################################
#### RERUN EVERYTHING FOR 1 WEEK DATA ####
##########################################
  



if (FALSE) {
  cat("\014")
  
  # write_csv(estimation_data, "../data_1week_sample05_ne.csv")
  
  # %>% 
  #  filter_all(any_vars(is.na(.)))
  

}

  
## Working RAW_VS_DESEASONED plots
if (FALSE) {
  
  sum_consumption_def_ne <- 
    Deflate_Than_Sum(consumption_ne)
  sum_consumption_ds_def_ne <- 
    Deflate_Than_Sum(consumption_ds_ne)
  
  sum_consumption_def_mw <- 
    Deflate_Than_Sum(consumption_mw)
  sum_consumption_ds_def_mw <- 
    Deflate_Than_Sum(consumption_ds_mw)
  
  sum_consumption_def_so <- 
    Deflate_Than_Sum(consumption_so)
  sum_consumption_ds_def_so <- 
    Deflate_Than_Sum(consumption_ds_so)
  
  sum_consumption_def_we <- 
    Deflate_Than_Sum(consumption_we)
  sum_consumption_ds_def_we <- 
    Deflate_Than_Sum(consumption_ds_we)
  
  
  plot_ne <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_ne %>%
                select(DATE, AVG_TRIP_3M_NE, TREND_1Y_NE) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_ds_ne <- 
    ggplot() + 
    geom_line(data = sum_consumption_ds_def_ne %>%
                select(DATE, AVG_TRIP_3M_NE, TREND_1Y_NE) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_mw <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_mw %>%
                select(DATE, AVG_TRIP_3M_MW, TREND_1Y_MW) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_ds_mw <- 
    ggplot() + 
    geom_line(data = sum_consumption_ds_def_mw %>%
                select(DATE, AVG_TRIP_3M_MW, TREND_1Y_MW) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_so <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_so %>%
                select(DATE, AVG_TRIP_3M_SO, TREND_1Y_SO) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_ds_so <- 
    ggplot() + 
    geom_line(data = sum_consumption_ds_def_so %>%
                select(DATE, AVG_TRIP_3M_SO, TREND_1Y_SO) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_we <- 
    ggplot() + 
    geom_line(data = sum_consumption_def_we %>%
                select(DATE, AVG_TRIP_3M_WE, TREND_1Y_WE) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  plot_ds_we <- 
    ggplot() + 
    geom_line(data = sum_consumption_ds_def_we %>%
                select(DATE, AVG_TRIP_3M_WE, TREND_1Y_WE) %>% 
                filter(between(DATE,
                               as.Date("2004-06-01"), 
                               as.Date("2014-06-01"))) %>% 
                pivot_longer(-DATE),
              aes(x = DATE, 
                  y = value, 
                  colour = name)) +
    scale_x_date(date_breaks = "1 year",
                 date_labels = "%Y") +
    theme(axis.title.x = element_blank(), axis.text.x = element_text())
  
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(plot_ne),
                  ggplotGrob(plot_ds_ne),
                  ggplotGrob(plot_mw),
                  ggplotGrob(plot_ds_mw),
                  ggplotGrob(plot_so),
                  ggplotGrob(plot_ds_so),
                  ggplotGrob(plot_we),
                  ggplotGrob(plot_ds_we),
                  size = "last"))
  
}