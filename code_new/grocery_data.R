
library(tidyverse)

base.path <- '/extra/agalvao/eis_nielsen'

# We have data from 2004 to 2017
years <- seq(2004, 2017)


############################################
### These are the columns we want to return from each dataset

# Columns to use from the Panelists data
panelistsCols <-  
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
panelistsColsNew <- 
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
tripsCols <- 
  c('trip_code_uc', 
    'household_code', 
    'retailer_code', 
    'purchase_date', 
    'panel_year', 
    'total_spent')

# Columns to use from the Retailer data
retailersCols <- 
  c('retailer_code', 
    'channel_type')

# Columns to use when combining Trips and Panelists
tripsPanelistsCols <- unique(c(tripsCols, panelistsColsNew))

# Columns to pull out while aggregating over total_spent
groceryTripsIndex <- 
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
groceryTripsCols <- 
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
tripsPanelists <- 
  setNames(data.frame(matrix(ncol = length(tripsPanelistsCols),
                             nrow = 0)),
           tripsPanelistsCols)

# Loop through all the years to pull in the data
# The data is in separate sub-folders for each year
for (ii in length(years)) {
  
  # Select year
  year <- years[ii]
  
  # Get panelists file and select columns
  # This file contains household characteristics
  panelistsFileName <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  
  panelistsTemp <- 
    read_tsv(panelistsFileName) %>% 
    select(panelistsCols)
  
  # Rename column names for consistency
  colnames(panelistsTemp) <- panelistsColsNew
  
  # Get trips file and select columns
  # This file contains the total amount spent per trip
  # There may be multiple trips per day
  tripsFileName <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/trips_", 
                     year, 
                     ".tsv"))
  
  tripsTemp <- 
    read_tsv(tripsFileName) %>% 
    select(tripsCols)
  
  # Join trips and panelists together
  tripsPanelistsTemp <- 
    tripsTemp %>%
    left_join(panelistsTemp, 
              by=c('household_code'='household_code', 
                   'panel_year'='panel_year')) %>%
    select(tripsPanelistsCols)
    # Arrange data as desired
  
  # Bind data together from previous years
  tripsPanelists <- 
    rbind(tripsPanelists, 
          tripsPanelistsTemp)
}


############################################
### Construct final data

# Get retailer data and select columns
# The retailer data is necessary to filter by grocery stores
retailersFileName <- 
  file.path(base.path, 
            paste0("nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))

retailers <- 
  read_tsv(retailersFileName) %>% 
  select(retailersCols)

# Join retailer data to trips data
trips <- 
  tripsPanelists %>% 
  left_join(retailers, by='retailer_code')

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
  select(groceryTripsIndex) %>%
  distinct()

# Join the household characteristics back to the aggregated daily consumption
GroceryTrips <- 
  Consumption %>%
  left_join(groceryConst, 
            by=c("household_code", 
                 "purchase_date", 
                 "panel_year")) %>%
  select(groceryTripsCols)

# How does it look?
head(GroceryTrips)
nrow(GroceryTrips)

# Write to file
groceryFileName <- 
  file.path(base.path, 
            "Datasets/GroceryTrips.csv")

write_csv(GroceryTrips, 
          groceryFileName)