rm(list=ls())

library(tidyverse)

base.path <- '/extra/agalvao/eis_nielsen'

# We have data from 2004 to 2017
years <- seq(2004, 2016)

# Initialize variable
households <- NULL


# We need a vector containing all household codes
for (i in 1:length(years)) {
  
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
  
  # Bind data together from previous years
  households <- 
    c(households,
      households_temp)
  
  #print(year)
  #print(length(households_temp))

}

sprintf("Total number of households: %i", length(households))
sprintf("Unique households: %i", length(unique(households)))

# Control what is sampled
set.seed(1)

# Percentage of households to sample
factor = 0.01

# Select which households we will track
households_sample <- 
  unique(households) %>% 
  sample(factor * length(unique(households)))

# Check it out
print("glimpse(households_sample) :")
glimpse(households_sample)


# Create panelist data sample
for (i in 1:length(years)) {
  
  # Select year
  year <- years[i]
  
  panelists_filename <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  trips_filename <- 
    file.path(base.path, 
              paste0('nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/trips_", 
                     year, 
                     ".tsv"))
  
  # Reduces tsv file to samples
  panelists_sample <- 
    read_tsv(panelists_filename,
             col_types = cols(.default = "c")) %>% 
    filter(Household_Cd %in% households_sample)
  
  trips_sample <- 
    read_tsv(trips_filename,
             col_types = cols(.default = "c")) %>% 
    filter(household_code %in% households_sample)
  
  panelists_directory <- 
    file.path(base.path, 
              paste0('sample_nielsen/nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files"))
  
  if (!dir.exists(panelists_directory)) 
    dir.create(panelists_directory, 
               recursive = TRUE)
  
  # Write to file
  panelists_sample_filename <- 
    file.path(base.path, 
              paste0('sample_nielsen/nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/panelists_", 
                     year, 
                     ".tsv"))
  
  trips_sample_filename <- 
    file.path(base.path, 
              paste0('sample_nielsen/nielsen_extracts/HMS/', 
                     year, 
                     "/Annual_Files/trips_", 
                     year, 
                     ".tsv"))
  
  write_tsv(panelists_sample, 
            panelists_sample_filename,
            na = "")
  
  write_tsv(trips_sample, 
            trips_sample_filename,
            na = "")
}