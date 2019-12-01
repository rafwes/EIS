# cd '/extra/agalvao/eis_nielsen'

library(data.table)
library(tidyverse)

Years <- c('2004', '2005', '2006', '2007','2008', '2009', '2010', '2011', '2012', '2013', '2014')
Years <- c('2004', '2005')

panelistsCols <- c('Household_Cd', 'Panel_Year', 'Projection_Factor', 'Projection_Factor_Magnet', 'Household_Income', 'Household_Size', 'Type_Of_Residence', 'Male_Head_Age', 'Female_Head_Age', 'Male_Head_Education', 'Female_Head_Education', 'Male_Head_Occupation', 'Female_Head_Occupation', 'Male_Head_Employment', 'Female_Head_Employment', 'Marital_Status', 'Race', 'Hispanic_Origin', 'Fips_State_Desc')

panelistsColsNew <- c('household_code', 'panel_year', 'projection_factor', 'projection_factor_magnet', 'household_income', 'household_size', 'type_of_residence', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'male_head_occupation', 'female_head_occupation', 'male_head_employment', 'female_head_employment', 'marital_status', 'race', 'hispanic_origin', 'fips_state_descr')

tripsCols <- c('household_code', 'panel_year', 'trip_code_uc', 'retailer_code', 'purchase_date', 'total_spent')

retailersCols <- c('retailer_code', 'channel_type')

pandtCols <- c(panelistsColsNew, tripsCols[-(1:2)])

Trips <- data.table(1)[,`:=`(eval(pandtCols),NA)][,V1:=NULL][.0]

for (ii in length(Years)) {
  paneliststemp <- fread(paste0("nielsen_extracts/HMS/", Years[ii], "/Annual_Files/panelists_", Years[ii], ".tsv"), select = panelistsCols)
  colnames(paneliststemp) <- panelistsColsNew
  
  tripstemp <- fread(paste0("nielsen_extracts/HMS/", Years[ii], "/Annual_Files/trips_", Years[ii], ".tsv"), select=tripsCols)
  
  pandt <- left_join(paneliststemp, tripstemp, by = c("household_code", "panel_year"))
  
  print(head(pandt))
  print(ncol(pandt))
  
  Trips <- rbindlist(list(Trips, pandt), use.names=TRUE)
}

print(head(Trips))
print(ncol(Trips))

fwrite(head(Trips), "EIS/test.csv")