# cd '/extra/agalvao/eis_nielsen/nielsen_extracts'

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

Years = ['2004', '2005', '2006', '2007','2008', '2009', '2010', '2011', '2012', '2013', '2014']
panelistsCols = ['Household_Cd', 'Panel_Year', 'Projection_Factor', 'Projection_Factor_Magnet', 'Household_Income', 'Household_Size', 'Type_Of_Residence', 'Male_Head_Age', 'Female_Head_Age', 'Male_Head_Education', 'Female_Head_Education', 'Male_Head_Occupation', 'Female_Head_Occupation', 'Male_Head_Employment', 'Female_Head_Employment', 'Marital_Status', 'Race', 'Hispanic_Origin', 'Fips_State_Desc']
panelistsColsNew = ['household_code', 'panel_year', 'projection_factor', 'projection_factor_magnet', 'household_income', 'household_size', 'type_of_residence', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'male_head_occupation', 'female_head_occupation', 'male_head_employment', 'female_head_employment', 'marital_status', 'race', 'hispanic_origin', 'fips_state_descr']
tripsCols = ['trip_code_uc', 'household_code', 'retailer_code', 'purchase_date', 'panel_year', 'total_spent']
retailersCols = ['retailer_code', 'channel_type']
#purchasesCols = ['trip_code_uc', 'upc', 'upc_ver_uc', 'quantity', 'total_price_paid']



Trips = pd.DataFrame()
for ii in range(len(Years)):
    paneliststemp = pd.read_csv("HMS/"+Years[ii]+"/Annual_Files/panelists_"+Years[ii]+".tsv", sep='\t', usecols=panelistsCols)
    paneliststemp.columns = panelistsColsNew
    #paneliststemp = paneliststemp[paneliststemp.projection_factor_magnet > 0]
    #paneliststemp = paneliststemp.reset_index(drop=True)
    tripstemp = pd.read_csv("HMS/"+Years[ii]+"/Annual_Files/trips_"+Years[ii]+".tsv", sep='\t', usecols=tripsCols)
    pandt = pd.merge(paneliststemp, tripstemp, how='left', on=['household_code', 'panel_year'])
    pandt = pandt.reset_index(drop=True)
    paneliststemp = []
    tripstemp = []
    Trips = Trips.append(pandt)

Trips = Trips.reset_index(drop=True)

retailerstemp = pd.read_csv("HMS/Master_Files/Latest/retailers.tsv", sep='\t', usecols=retailersCols)
TripsR = pd.merge(Trips, retailerstemp, how='left', on='retailer_code')
TripsR = TripsR.reset_index(drop=True)

GTripsR = TripsR[TripsR.channel_type == 'Grocery']
GTripsR = GTripsR.reset_index(drop=True)

############################################################################

IRData = pd.read_csv("EIS/UseData/IRData.csv", sep=",")
#IRData = pd.read_csv("UseData/IRData.csv", sep=",")

Northeast = ['ME', 'VT', 'NH', 'MA', 'RI', 'CT', 'NJ', 'NY', 'PA']
Midwest = ['OH', 'MI', 'IN', 'IL', 'WI', 'MN', 'IA', 'MO', 'ND', 'SD', 'NE', 'KS']
South = ['MD', 'DE', 'DC', 'WV', 'VA', 'NC', 'SC', 'GA', 'FL', 'KY', 'TN', 'AL', 'MS', 'AR', 'LS', 'OK', 'TX']
West = ['MT', 'WY', 'CO', 'NM', 'ID', 'UT', 'AZ', 'NV', 'WA', 'OR', 'CA', 'AK', 'HI']

def AddRegion(row):
    if (row['fips_state_descr'] in Northeast): 
        return 'Northeast'
    elif (row['fips_state_descr'] in Midwest):
        return 'Midwest'
    elif (row['fips_state_descr'] in South):
        return 'South'
    elif (row['fips_state_descr'] in West):
        return 'West'
    else:
        return
        
GTripsR['region'] = GTripsR.apply(AddRegion, axis=1)

GTripsR['purchase_date'] =  pd.to_datetime(GTripsR['purchase_date'], format='%Y-%m-%d')
GTripsR['year'] = (GTripsR['purchase_date']).dt.year
GTripsR['month'] = (GTripsR['purchase_date']).dt.month

GroceryTrips = pd.merge(GTripsR, IRData, how='left', on=['year', 'month', 'region'])

GroceryTrips.to_csv("../Datasets/GroceryTrips.csv", index=False)



