rm(list=ls())

library(tidyverse)

#base_path <- "/extra/agalvao/eis_nielsen"
base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

# We have data from 2004 to 2017
years <- seq(2004, 2017)


############################################
### These are the columns we want to return from each dataset

# Columns to use from the Panelists data
panelists_cols <-  
  c("Household_Cd", 
    "Panel_Year", 
    "Projection_Factor", 
    "Projection_Factor_Magnet", 
    "Household_Income", 
    "Household_Size", 
    "Type_Of_Residence", 
    "Male_Head_Age", 
    "Female_Head_Age", 
    "Male_Head_Education", 
    "Female_Head_Education", 
    "Male_Head_Occupation", 
    "Female_Head_Occupation", 
    "Male_Head_Employment", 
    "Female_Head_Employment", 
    "Marital_Status", 
    "Race", 
    "Hispanic_Origin", 
    "Fips_State_Desc")

# Columns to rename the panelists data for consistency
panelists_cols_new <- 
  c("HOUSEHOLD_CODE", 
    "PANEL_YEAR", 
    "PROJECTION_FACTOR", 
    "PROJECTION_FACTOR_MAGNET", 
    "HOUSEHOLD_INCOME", 
    "HOUSEHOLD_SIZE", 
    "TYPE_OF_RESIDENCE", 
    "MALE_HEAD_AGE", 
    "FEMALE_HEAD_AGE", 
    "MALE_HEAD_EDUCATION", 
    "FEMALE_HEAD_EDUCATION", 
    "MALE_HEAD_OCCUPATION", 
    "FEMALE_HEAD_OCCUPATION", 
    "MALE_HEAD_EMPLOYMENT", 
    "FEMALE_HEAD_EMPLOYMENT", 
    "MARITAL_STATUS", 
    "RACE", 
    "HISPANIC_ORIGIN", 
    "FIPS_STATE_DESCR")

# Columns to rename trips data for consistency
trips_cols <- 
  c("trip_code_uc", 
    "household_code", 
    "retailer_code", 
    "purchase_date", 
    "panel_year", 
    "total_spent")

# Columns to rename trips data for consistency
trips_cols_new <- 
  c("TRIP_CODE_UC", 
    "HOUSEHOLD_CODE", 
    "RETAILER_CODE", 
    "PURCHASE_DATE", 
    "PANEL_YEAR", 
    "TOTAL_SPENT")

# Columns to use from the Retailer data
retailers_cols <- 
  c("retailer_code", 
    "channel_type")

# Columns to use from the Retailer data
retailers_cols_new <- 
  c("RETAILER_CODE", 
    "CHANNEL_TYPE")


northeast = c("ME", "VT", "NH", "MA", "RI", "CT", "NJ", "NY", "PA")
midwest = c("OH", "MI", "IN", "IL", "WI", "MN", "IA", "MO", "ND", "SD", "NE", "KS")
south = c("MD", "DE", "DC", "WV", "VA", "NC", "SC", "GA", "FL", "KY", "TN", "AL", "MS", "AR", "LS", "OK", "TX")
west = c("MT", "WY", "CO", "NM", "ID", "UT", "AZ", "NV", "WA", "OR", "CA", "AK", "HI")


## ATTENTION, ONLY 2017!!!!!!
for (i in length(years)) {
  
  # Select year
  year <- years[i]
  
  # Get panelists file and select columns
  # This file contains household characteristics
  panelists_filename <- 
    file.path(base_path, 
              paste0("nielsen_extracts/HMS/", 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  
  panelists <- 
    read_tsv(panelists_filename) %>% 
    select(panelists_cols)
  
  
  # Rename column names for consistency
  colnames(panelists) <- panelists_cols_new
  
  # Get trips file and select columns
  # This file contains the total amount spent per trip
  # There may be multiple trips per day
  trips_filename <- 
    file.path(base_path, 
              paste0("nielsen_extracts/HMS/", 
                     year, 
                     "/Annual_Files/trips_", 
                     year, 
                     ".tsv"))
  
  trips <- 
    read_tsv(trips_filename) %>% 
    select(trips_cols)
  
  # Rename column names for consistency
  colnames(trips) <- trips_cols_new
  
  # Get retailer data and select columns
  # The retailer data is necessary to filter by grocery stores
  retailers_filename <- 
    file.path(base_path, 
              paste0("nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))
  
  retailers <- 
    read_tsv(retailers_filename) %>% 
    select(retailers_cols)
  
  # Rename column names for consistency
  colnames(retailers) <- retailers_cols_new
  
  # Join retailer data to trips data
  trips_retailers <- 
    trips %>% 
    left_join(retailers, 
              by="RETAILER_CODE")
  
  rm(retailers,trips)
  
  # Multiple trips a day by the same household are summed up
  consumption <- 
    trips_retailers %>%
    filter(CHANNEL_TYPE == "Grocery") %>% 
    select(HOUSEHOLD_CODE, 
           PURCHASE_DATE, 
           PANEL_YEAR, 
           TOTAL_SPENT) %>%
    group_by(HOUSEHOLD_CODE, PANEL_YEAR, PURCHASE_DATE) %>%
    summarise(TOTAL_SPENT = sum(TOTAL_SPENT)) %>%
    ungroup()
  
  rm(trips_retailers)
  
  # Gather region from panelists and join to consumption
  
  consumption <-
    consumption %>% 
    left_join(panelists %>% 
                select(HOUSEHOLD_CODE,
                       FIPS_STATE_DESCR),
              by = "HOUSEHOLD_CODE")
  
  consumption_ne <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% northeast) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  consumption_mw <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% midwest) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  consumption_so <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% south) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  consumption_we <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% west) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  #rm(consumption)
  
}

rm(i,
   year,
   years,
   panelists_filename,
   panelists_cols, 
   panelists_cols_new,
   retailers_filename,
   retailers_cols,
   retailers_cols_new,
   trips_filename,
   trips_cols,
   trips_cols_new)



a <- rbind(consumption_mw,
          consumption_ne,
          consumption_so,
          consumption_we)

b <- anti_join(consumption,a)


#######################################################################

if (FALSE) {

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
for (i in 1:length(years)) {
  
  # Select year
  year <- years[i]
  
  # Get panelists file and select columns
  # This file contains household characteristics
  panelists_filename <- 
    file.path(base_path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  
  panelists <- 
    read_tsv(panelists_filename) %>% 
    select(panelists_cols)
  
  # Rename column names for consistency
  colnames(panelists) <- panelists_cols_new
  
  # Get trips file and select columns
  # This file contains the total amount spent per trip
  # There may be multiple trips per day
  trips_filename <- 
    file.path(base_path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/trips_", 
                     year, 
                     ".tsv"))
  
  trips <- 
    read_tsv(trips_filename) %>% 
    select(trips_cols)
  
  # Join trips and panelists together
  trips_panelists_temp <- 
    trips %>%
    left_join(panelists, 
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
  file.path(base_path, 
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
only_grocery_trips <- 
  trips %>%
  filter(channel_type == 'Grocery')

# How does it look?
head(only_grocery_trips)
nrow(only_grocery_trips)

# Aggregate from trip level to daily level
consumption <- 
  only_grocery_trips %>%
  select(household_code, 
         purchase_date, 
         panel_year, 
         total_spent) %>%
  group_by(household_code, panel_year, purchase_date) %>%
  summarise(total_spent = sum(total_spent)) %>%
  ungroup()

# Pull out the household characteristics that don't change over the year
grocery_const <- 
  only_grocery_trips %>%
  select(grocery_trips_index) %>%
  distinct()

# Join the household characteristics back to the aggregated daily consumption
grocery_trips <- 
  consumption %>%
  left_join(grocery_const, 
            by=c("household_code", 
                 "purchase_date", 
                 "panel_year")) %>%
  select(grocery_trips_cols)

# How does it look?
head(grocery_trips)
nrow(grocery_trips)

# Write to file
grocery_filename <- 
  file.path(base_path, 
            "Datasets/grocery_trips.csv")

write_csv(grocery_trips, 
          grocery_filename)


}
