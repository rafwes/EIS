#rm(list=ls())

library(tidyverse)
library(visdat)
library(naniar)

base_path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
#base_path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

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


# Data will be later separated by regions
northeast <-
  c("ME", "VT", "NH", 
    "MA", "RI", "CT", 
    "NJ", "NY", "PA")

midwest <- 
  c("OH", "MI", "IN", 
    "IL", "WI", "MN", 
    "IA", "MO", "ND", 
    "SD", "NE", "KS")

south <- 
  c("MD", "DE", "DC", 
    "WV", "VA", "NC", 
    "SC", "GA", "FL", 
    "KY", "TN", "AL", 
    "MS", "AR", "LS", 
    "OK", "TX", "LA")

west <- 
  c("MT", "WY", "CO", 
    "NM", "ID", "UT", 
    "AZ", "NV", "WA", 
    "OR", "CA", "AK", 
    "HI")


panelists_cols_type <- 
  cols(
    .default = col_double(),
    Panelist_ZipCd = col_character(),
    Fips_State_Desc = col_character(),
    Fips_County_Desc = col_character(),
    Scantrack_Market_Identifier_Desc = col_character(),
    DMA_Name = col_character(),
    Wic_Indicator_Current = col_logical(),
    Wic_Indicator_Ever_Not_Current = col_logical(),
    Member_4_Employment = col_logical(),
    Member_5_Employment = col_logical(),
    Member_6_Birth = col_character(),
    Member_6_Relationship_Sex = col_character(),
    Member_6_Employment = col_logical(),
    Member_7_Birth = col_character(),
    Member_7_Relationship_Sex = col_character(),
    Member_7_Employment = col_logical()
  )

trips_cols_type <- 
  cols(
    trip_code_uc = col_double(),
    household_code = col_double(),
    purchase_date = col_date(format = ""),
    retailer_code = col_double(),
    store_code_uc = col_double(),
    panel_year = col_double(),
    store_zip3 = col_character(),
    total_spent = col_double()
  )

retailer_cols_type <- 
  cols(
    retailer_code = col_double(),
    channel_type = col_character()
  )


# Get retailer data and select columns
# The retailer data is necessary to filter by grocery stores
retailers_filename <- 
  file.path(base_path, 
            paste0("nielsen_extracts/HMS/Master_Files/Latest/retailers.tsv"))

retailers <- 
  read_tsv(retailers_filename,
           col_types = retailer_cols_type) %>% 
  select(retailers_cols)

# Rename column names for consistency
colnames(retailers) <- retailers_cols_new

## Import all consumption data over the years
for (i in 1:length(years)) {
  
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
  
  panelists_year <- 
    read_tsv(panelists_filename,
             col_types = panelists_cols_type) %>% 
    select(panelists_cols)
  
  
  # Rename column names for consistency
  colnames(panelists_year) <- panelists_cols_new
  
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
    read_tsv(trips_filename,
             col_types = trips_cols_type) %>% 
    select(trips_cols)
  
  # Rename column names for consistency
  colnames(trips) <- trips_cols_new

  
  # Join retailer data to trips data
  trips_retailers <- 
    trips %>% 
    left_join(retailers, 
              by="RETAILER_CODE")
  
  rm(trips)
  
  # Multiple trips a day by the same household are summed up
  # Total spent cannot be zero, drop erroneous data.
  consumption <- 
    trips_retailers %>%
    filter(CHANNEL_TYPE == "Grocery") %>% 
    select(HOUSEHOLD_CODE, 
           PURCHASE_DATE, 
           PANEL_YEAR, 
           TOTAL_SPENT) %>%
    group_by(HOUSEHOLD_CODE, PANEL_YEAR, PURCHASE_DATE) %>%
    summarise(TOTAL_SPENT = sum(TOTAL_SPENT)) %>%
    ungroup() %>% 
    filter(TOTAL_SPENT != 0)
  
  rm(trips_retailers)
  
  # Gather region from panelists and join to consumption
  consumption <-
    consumption %>% 
    left_join(panelists_year %>% 
                select(HOUSEHOLD_CODE,
                       FIPS_STATE_DESCR),
              by = "HOUSEHOLD_CODE")
  
  # We need to deflate prices by region, so separate them
  consumption_ne_year <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% northeast) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  consumption_mw_year <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% midwest) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  consumption_so_year <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% south) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  consumption_we_year <- 
    consumption %>%
    filter(FIPS_STATE_DESCR %in% west) %>% 
    select(HOUSEHOLD_CODE,
           PURCHASE_DATE,
           TOTAL_SPENT)
  
  rm(consumption)
  
  # Agregate yearly data into single dataframes
  if (exists("consumption_mw")) {
    consumption_mw <- 
      rbind(consumption_mw,
            consumption_mw_year)
  } else {
    consumption_mw <- consumption_mw_year
    }

  if (exists("consumption_ne")) {
    consumption_ne <- 
      rbind(consumption_ne,
            consumption_ne_year)
  } else {
    consumption_ne <- consumption_ne_year
    }

  if (exists("consumption_so")) {
    consumption_so <- 
      rbind(consumption_so,
            consumption_so_year)
  } else {
    consumption_so <- consumption_so_year
    }
  
  if (exists("consumption_we")) {
    consumption_we <- 
      rbind(consumption_we,
            consumption_we_year)
  } else {
    consumption_we <- consumption_we_year
    }
  
  # Do the same for panelists
  
  if (exists("panelists")) {
    panelists <- 
      rbind(panelists,
            panelists_year)
  } else {
    panelists <- panelists_year
  }
  
}

rm(i,
   year,
   years,
   panelists_filename,
   panelists_cols, 
   panelists_cols_new,
   panelists_cols_type,
   retailers_filename,
   retailers_cols,
   retailers_cols_new,
   retailer_cols_type,
   trips_filename,
   trips_cols,
   trips_cols_new,
   trips_cols_type,
   midwest,
   south,
   northeast,
   west,
   consumption_mw_year,
   consumption_ne_year,
   consumption_so_year,
   consumption_we_year,
   panelists_year,
   retailers
   )


#######################################################################

if (FALSE) {

# Write to file
grocery_filename <- 
  file.path(base_path, 
            "Datasets/grocery_trips.csv")

write_csv(grocery_trips, 
          grocery_filename)

}


if (FALSE) {
  
  ###################################
  # Arrange consumption data into time series
  
  groceries_daily_mw <- data.frame(seq(as.Date('2003-12-20'), as.Date('2018-01-30'), by="days"))
  groceries_daily_ne <- data.frame(seq(as.Date('2003-12-20'), as.Date('2018-01-30'), by="days"))
  groceries_daily_so <- data.frame(seq(as.Date('2003-12-20'), as.Date('2018-01-30'), by="days"))
  groceries_daily_we <- data.frame(seq(as.Date('2003-12-20'), as.Date('2018-01-30'), by="days"))
  colnames(groceries_daily_mw) <- "PURCHASE_DATE"
  colnames(groceries_daily_ne) <- "PURCHASE_DATE" 
  colnames(groceries_daily_so) <- "PURCHASE_DATE" 
  colnames(groceries_daily_we) <- "PURCHASE_DATE" 
  
  consumption_ts_mw <- consumption_mw %>% 
    group_by(HOUSEHOLD_CODE) %>%
    group_split()
  
  consumption_ts_ne <- consumption_ne %>% 
    group_by(HOUSEHOLD_CODE) %>%
    group_split()
  
  consumption_ts_so <- consumption_so %>% 
    group_by(HOUSEHOLD_CODE) %>%
    group_split()
  
  consumption_ts_we <- consumption_we %>% 
    group_by(HOUSEHOLD_CODE) %>%
    group_split()
  
  for (i in 1:length(consumption_ts_mw)) {
    
    df <- data.frame(consumption_ts_mw[i])
    colname <- as.character(df[[1]][1])
    df <- rename(df, !!colname := "TOTAL_SPENT")
    df <- df %>% select(- HOUSEHOLD_CODE)
    
    groceries_daily_mw <-
      groceries_daily_mw %>% 
      left_join(df, by = "PURCHASE_DATE")
    
  }
  
  for (i in 1:length(consumption_ts_ne)) {
    
    df <- data.frame(consumption_ts_ne[i])
    colname <- as.character(df[[1]][1])
    df <- rename(df, !!colname := "TOTAL_SPENT")
    df <- df %>% select(- HOUSEHOLD_CODE)
    
    groceries_daily_ne <-
      groceries_daily_ne %>% 
      left_join(df, by = "PURCHASE_DATE")
    
  }
  
  for (i in 1:length(consumption_ts_so)) {
    
    df <- data.frame(consumption_ts_so[i])
    colname <- as.character(df[[1]][1])
    df <- rename(df, !!colname := "TOTAL_SPENT")
    df <- df %>% select(- HOUSEHOLD_CODE)
    
    groceries_daily_so <-
      groceries_daily_so %>% 
      left_join(df, by = "PURCHASE_DATE")
    
  }
  
  for (i in 1:length(consumption_ts_we)) {
    
    df <- data.frame(consumption_ts_we[i])
    colname <- as.character(df[[1]][1])
    df <- rename(df, !!colname := "TOTAL_SPENT")
    df <- df %>% select(- HOUSEHOLD_CODE)
    
    groceries_daily_we <-
      groceries_daily_we %>% 
      left_join(df, by = "PURCHASE_DATE")
    
  }
  
  
  
  #vis_dat(groceries_daily, warn_large_data = FALSE)
  
  #vis_miss(groceries_daily_mw, warn_large_data = FALSE)
  #vis_miss(groceries_daily_ne, warn_large_data = FALSE)
  #vis_miss(groceries_daily_so, warn_large_data = FALSE)
  #vis_miss(groceries_daily_we, warn_large_data = FALSE)
  
  
  rm(df,
     consumption_ts_mw,
     consumption_ts_ne,
     consumption_ts_so,
     consumption_ts_we)
  
}