

library(tidyverse)

base.path <- '/extra/agalvao/eis_nielsen'

# We have data from 2004 to 2017
years <- seq(2004, 2017)

# These are the columns we want to return from each dataset
panelistsCols = c('Household_Cd', 'Panel_Year', 'Projection_Factor', 'Projection_Factor_Magnet', 'Household_Income', 'Household_Size', 'Type_Of_Residence', 'Male_Head_Age', 'Female_Head_Age', 'Male_Head_Education', 'Female_Head_Education', 'Male_Head_Occupation', 'Female_Head_Occupation', 'Male_Head_Employment', 'Female_Head_Employment', 'Marital_Status', 'Race', 'Hispanic_Origin', 'Fips_State_Desc')

panelistsColsNew = c('household_code', 'panel_year', 'projection_factor', 'projection_factor_magnet', 'household_income', 'household_size', 'type_of_residence', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'male_head_occupation', 'female_head_occupation', 'male_head_employment', 'female_head_employment', 'marital_status', 'race', 'hispanic_origin', 'fips_state_descr')

tripsCols <- c('trip_code_uc', 'household_code', 'retailer_code', 'purchase_date', 'panel_year', 'total_spent')

retailersCols <- c('retailer_code', 'channel_type')

tripsPanelistsCols <- unique(c(tripsCols, panelistsColsNew))

groceryTripsIndex <- c('household_code', 'purchase_date', 'panel_year', 'projection_factor', 'projection_factor_magnet', 'household_income', 'household_size', 'type_of_residence', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'male_head_occupation', 'female_head_occupation', 'male_head_employment', 'female_head_employment', 'marital_status', 'race', 'hispanic_origin', 'fips_state_descr')

groceryTripsCols <- c('household_code', 'purchase_date', 'panel_year', 'total_spent', 'projection_factor', 'projection_factor_magnet', 'household_income', 'household_size', 'type_of_residence', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'male_head_occupation', 'female_head_occupation', 'male_head_employment', 'female_head_employment', 'marital_status', 'race', 'hispanic_origin', 'fips_state_descr')

# Create empty matrix
tripsPanelists <- setNames(data.frame(matrix(ncol = length(tripsPanelistsCols), nrow = 0)), tripsPanelistsCols)

# Loop through all the years
for (ii in length(years)) {
  
  # Select year
  year <- years[ii]
  
  # Get panelists file and select columns
  panelistsFileName <- file.path(base.path, paste0('nielsen_extracts/HMS/', year, "/Annual_Files/panelists_", year, ".tsv"))
  panelistsTemp <- read_tsv(panelistsFileName) %>%
    select(panelistsCols)
  
  # Rename column names
  colnames(panelistsTemp) <- panelistsColsNew
  
  # Get trips file and select columns
  tripsFileName <- file.path(base.path, paste0('nielsen_extracts/HMS/', year, "/Annual_Files/trips_", year, ".tsv"))
  tripsTemp <- read_tsv(tripsFileName) %>%
    select(tripsCols)
  
  # Join trips and panelists together
  tripsPanelistsTemp <- tripsTemp %>%
    left_join(panelistsTemp, by=c('household_code'='household_code', 'panel_year'='panel_year'))
  
  # Arrange data as planned
  tripsPanelistsTemp <- tripsPanelistsTemp %>%
    select(tripsPanelistsCols)
  
  # Bind trips data together
  tripsPanelists <- rbind(tripsPanelists, tripsPanelistsTemp)
  
}

# Get retailer data and select columns
retailersFileName <- file.path(base.path, paste0("nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))
retailers <- read_tsv(retailersFileName) %>%
  select(retailersCols)

# Join retailer data to trips data
trips <- tripsPanelists %>%
  left_join(retailers, by='retailer_code')

# Restrict for grocery purchases
tripsGrocery <- trips %>%
  filter(channel_type == 'Grocery')

# How does it look?
head(tripsGrocery)
nrow(tripsGrocery)

# Aggregate from trip level to daily level
Consumption <- tripsGrocery %>%
  select(household_code, purchase_date, panel_year, total_spent) %>%
  group_by(household_code, panel_year, purchase_date) %>%
  summarise(total_spent = sum(total_spent)) %>%
  ungroup()

GroceryTrips <- tripsGrocery %>%
  select(groceryTripsIndex) %>%
  left_join(Consumption, by=c("household_code", "purchase_date", "panel_year")) %>%
  select(groceryTripsCols)

# How does it look?
head(GroceryTrips)
nrow(GroceryTrips)

# Write to file
groceryFileName <- file.path(base.path, "Datasets/GroceryTrips.csv")
write_csv(GroceryTrips, groceryFileName)



