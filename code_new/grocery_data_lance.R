
library(tidyverse)

base.path <- '/extra/agalvao/eis_nielsen'

# We have data from 2004 to 2017
years <- seq(2004, 2017)


############################################
### These are the columns we want to return from each dataset

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

# Columns to use when combining Trips and Panelists
trips_panelists_cols <- unique(c(trips_cols, panelists_cols_new))

# Columns to pull out while aggregating over total_spent
grocery_trips_index <- 
  c('household_code', 
    'purchase_date', 
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

# Final dataset columns
grocery_trips_cols <- 
  c('household_code', 
    'purchase_date', 
    'panel_year', 
    'total_spent', 
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

############################################
### Pull in raw data

# Create empty matrix
trips_panelists <- 
  setNames(data.frame(matrix(ncol = length(trips_panelists_cols),
                             nrow = 0)),
           trips_panelists_cols)

# Loop through all the years to pull in the data
# The data is in separate sub-folders for each year
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


############################################
### Construct final data

# Get retailer data and select columns
# The retailer data is necessary to filter by grocery stores
retailers_filename <- 
  file.path(base.path, 
            paste0("nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))

retailers <- 
  read_tsv(retailers_filename) %>% 
  select(retailers_cols)

# Join retailer data to trips data
trips <- 
  trips_panelists %>% 
  left_join(retailers, 
            by='retailer_code')

# Restrict for grocery purchases
onlyGroceryTrips <- 
  trips %>%
  filter(channel_type == 'Grocery')

# How does it look?
head(onlyGroceryTrips)
nrow(onlyGroceryTrips)

# Aggregate from trip level to daily level
Consumption <- 
  onlyGroceryTrips %>%
  select(household_code, 
         purchase_date, 
         panel_year, 
         total_spent) %>%
  group_by(household_code, panel_year, purchase_date) %>%
  summarise(total_spent = sum(total_spent)) %>%
  ungroup()

# Pull out the household characteristics that don't change over the year
groceryConst <- 
  onlyGroceryTrips %>%
  select(grocery_trips_index) %>%
  distinct()

# Join the household characteristics back to the aggregated daily consumption
GroceryTrips <- 
  Consumption %>%
  left_join(groceryConst, 
            by=c("household_code", 
                 "purchase_date", 
                 "panel_year")) %>%
  select(grocery_trips_cols)

# How does it look?
head(GroceryTrips)
nrow(GroceryTrips)

# Write to file
groceryFileName <- 
  file.path(base.path, 
            "Datasets/GroceryTrips.csv")

write_csv(GroceryTrips, 
          groceryFileName)