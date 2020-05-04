rm(list=ls())

library(tidyverse)
library(plotrix)
#library(zoo)
#library(reshape2)
#library(ISOweek)
#library(lubridate)
#library(EnvStats)

#base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

monthly_file <- file.path(base_path,"csv_output/estimation_data_monthly.csv")
weekly_1w_file <- file.path(base_path,"csv_output/estimation_data_weekly_1w.csv")
weekly_4w_file <- file.path(base_path,"csv_output/estimation_data_weekly_4w.csv")


monthly_data <- read.csv(monthly_file)
weekly_1w_data <- read.csv(weekly_1w_file)
weekly_4w_data <- read.csv(weekly_4w_file)

rm(monthly_file,
   weekly_1w_file,
   weekly_4w_file)


print("Montly Data")
monthly_data %>%
  select("Y","X_TB","X_ST") %>%
  summarise_all(list(mean = mean, 
                     se = std.error,
                     median = median,
                     min = min,
                     max = max,
                     observ = length))

print("Weekly Data - 1 Week Aggregation")
weekly_1w_data %>%
  select("Y","X_TB","X_ST") %>%
  summarise_all(list(mean = mean, 
                     se = std.error,
                     median = median,
                     min = min,
                     max = max,
                     observ = length))

print("Weekly Data - 4 Weeks Aggregation")
weekly_4w_data %>%
  select("Y","X_TB","X_ST") %>%
  summarise_all(list(mean = mean, 
                     se = std.error,
                     median = median,
                     min = min,
                     max = max,
                     observ = length))