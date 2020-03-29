

library(tidyverse)

base.path <- '/extra/agalvao/eis_nielsen'

# We have data from 2004 to 2017
years <- seq(2004, 2017)
years <- seq(2004, 2005)

# These are the columns we want to return from each dataset
panelistsCols <- c('household_code', 'panel_year', 'projection_factor', 'projection_factor_magnet', 'household_income', 'household_size', 'type_of_residence', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'male_head_occupation', 'female_head_occupation', 'male_head_employment', 'female_head_employment', 'marital_status', 'race', 'hispanic_origin', 'fips_state_descr')

tripsCols <- c('trip_code_uc', 'household_code', 'retailer_code', 'purchase_date', 'panel_year', 'total_spent')

retailersCols <- c('retailer_code', 'channel_type')

tripsPanelistsCols <- unique(c(tripsCols, panelistsCols))

# Create empty matrix
tripsPanelists <- setNames(data.frame(matrix(ncol = 23, nrow = 0)), tripsPanelistsCols)

# Loop through all the years
for (ii in length(years)) {
  
  # Select year
  year <- years[ii]
  
  # Get panelists file
  panelistsFileName <- file.path(base.path, paste0('nielsen_extracts/HMS/', year, "/Annual_Files/panelists_", year, ".tsv"))
  panelistsTemp <- read_tsv(paneliststemp, col_names=panelistsCols)
  
  # Get trips file
  tripsFileName <- file.path(base.path, paste0('nielsen_extracts/HMS/', year, "/Annual_Files/trips_", year, ".tsv"))
  tripsTemp <- read_tsv(tripsFileName, col_names=tripsCols)
  
  # Join trips and panelists together
  tripsPanelistsTemp <- tripsTemp %>%
    left_join(panelistsTemp, by=c('household_code'='household_code', 'panel_year'='panel_year'))
  
  # Arrange data as planned
  tripsPanelistsTemp <- select(tripsPanelistsTemp, tripsPanelistsCols)
  
  # Bind trips data together
  tripsPanelists <- rbind(tripsPanelists, tripsPanelistsTemp)
  
}

# Get retailer data
retailersFileName <- file.path(base.path, paste0("nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))
retailers <- read_tsv(retailersFileName, col_names=retailersCols)

# Join retailer data to trips data
trips <- tripsPanelists %>%
  left_join(retailers, by='retailer_code')

# Restrict for grocery purchases
GroceryTrips <- trips %>%
  filter(channel_type == 'Grocery')

# How does it look?
head(GroceryTrips)

# Write to file
groceryFileName <- file.path(base.path, "Datasets/GroceryTrips.csv")
write_csv(GroceryTrips, groceryFileName)



