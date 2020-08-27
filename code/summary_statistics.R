rm(list=ls())

library(dplyr)
library(plotrix)

base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
#base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

quarterly_1q_grocery_file <- file.path(base_path,"csv_output/grocery_channel_no_income_cut/estimation_data_quarterly_1q.csv")
quarterly_1q_all_file <- file.path(base_path,"csv_output/all_channels_no_income_cut/estimation_data_quarterly_1q.csv")

quarterly_1q_grocery_data <- read.csv(quarterly_1q_grocery_file)
quarterly_1q_all_data <- read.csv(quarterly_1q_all_file)

cat("\nQuarterly Data (Grocery)\n")

output_numhh_grocery <- 
  quarterly_1q_grocery_data %>%
  select(HOUSEHOLD) %>% 
  unique %>% 
  count()

output_sumest_grocery <-
  quarterly_1q_grocery_data %>%
  select("Y","X_TB") %>%
  summarise_all(list(mean = mean, 
                     se = std.error,
                     median = median,
                     min = min,
                     max = max,
                     observ = length))

cat("Unique Housholds: ", output_numhh_grocery$n, "\n")
cat("Summary Statistics:\n")
print(output_sumest_grocery)


cat("\nQuarterly Data (All)\n")

output_numhh_all <- 
  quarterly_1q_all_data %>%
  select(HOUSEHOLD) %>% 
  unique %>% 
  count()

output_sumest_all <-
  quarterly_1q_all_data %>%
  select("Y","X_TB") %>%
  summarise_all(list(mean = mean, 
                     se = std.error,
                     median = median,
                     min = min,
                     max = max,
                     observ = length))

cat("Unique Housholds: ", output_numhh_all$n, "\n")
cat("Summary Statistics:\n")
print(output_sumest_all)