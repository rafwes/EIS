rm(list=ls())

library(tidyverse)

base.path <- '/extra/agalvao/eis_nielsen'

# We have data from 2004 to 2017
years <- seq(2004, 2016)

# Initialize variable
households <- NULL

for (i in length(years)) {
  
  # Select year
  year <- years[i]
  
  # This file contains household characteristics
  panelists_filename <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  
  households_temp <- 
    read_tsv(panelists_filename) %>% 
    select('Household_Cd') %>% 
    unlist() %>% 
    as.numeric()
  
  length(households_temp)
  
  # Bind data together from previous years

  households <- 
    c(households,
      households_temp)
}

## Control what is sampled
set.seed(1)

length(households)
length(unique(households))

# Select which households we will track
households_sample <- 
  unique(households) %>% 
  sample(15)

# check it out
households_sample


############# garbage bin
if(FALSE) {
  

  
  
  # Columns to use from the Panelists data
  panelists_cols <-  
    c('Household_Cd', 
      'Panel_Year', 
      'Projection_Factor', 
      'Projection_Factor_Magnet', 
      'Household_Income', 
      'Household_Size', 
      'Type_Of_Residence', 
      'Male_Head_Age', 
      'Female_Head_Age', 
      'Male_Head_Education', 
      'Female_Head_Education', 
      'Male_Head_Occupation', 
      'Female_Head_Occupation', 
      'Male_Head_Employment', 
      'Female_Head_Employment', 
      'Marital_Status', 
      'Race', 
      'Hispanic_Origin', 
      'Fips_State_Desc')
  
  # Columns to rename the panelists data for consistency
  panelists_cols_new <- 
    c('household_code', 
      'panel_year', 
      'projection_factor', 
      'projection_factor_magnet', 
      'household_income', 
      'household_size', 
      'type_of_residence', 
      'male_head_age', 
      'female_head_age', 
      'male_head_education', 
      'female_head_education', 
      'male_head_occupation', 
      'female_head_occupation', 
      'male_head_employment', 
      'female_head_employment', 
      'marital_status', 
      'race', 
      'hispanic_origin', 
      'fips_state_descr')
  
  
  # Columns to use from the Trips data
  trips_cols <- 
    c('trip_code_uc', 
      'household_code', 
      'retailer_code', 
      'purchase_date', 
      'panel_year', 
      'total_spent')
  
  # Columns to use from the Retailer data
  retailers_cols <- 
    c('retailer_code', 
      'channel_type')
  
  
  trips_panelists_cols <- unique(c(trips_cols, panelists_cols_new))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

for (i in length(years)) {
  
  # Select year
  year <- years[i]
  
  # Get panelists file and select columns
  # This file contains household characteristics
  panelists_filename <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  
  panelists_temp <- 
    read_tsv(panelists_filename) %>% 
    select(panelists_cols)
  
  # Rename column names for consistency
  colnames(panelists_temp) <- panelists_cols_new
  
  # Get trips file and select columns
  # This file contains the total amount spent per trip
  # There may be multiple trips per day
  trips_filename <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/trips_", 
                     year, 
                     ".tsv"))
  
  trips_temp <- 
    read_tsv(trips_filename) %>% 
    select(trips_cols)
  
  # Join trips and panelists together
  trips_panelists_temp <- 
    trips_temp %>%
    left_join(panelists_temp, 
              by=c('household_code'='household_code', 
                   'panel_year'='panel_year')) %>%
    select(trips_panelists_cols)
  
  # Bind data together from previous years
  trips_panelists <- 
    rbind(trips_panelists, 
          trips_panelists_temp)
}
  

  
  
}