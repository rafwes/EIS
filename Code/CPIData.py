# cd 'Documents/UIowa/Research/Nielsen/Habit Formation'

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

CPIfull = pd.read_csv("OtherData/CPI.txt", sep="\s+")
#CPIfull = pd.read_csv("../Data/OtherData/CPI.txt", sep="\s+")

CPIonID = CPIfull[(CPIfull['series_id'] == 'CUUR0100SA0') | (CPIfull['series_id'] == 'CUUR0200SA0') | (CPIfull['series_id'] == 'CUUR0300SA0') | (CPIfull['series_id'] == 'CUUR0400SA0')]

CPIonYear = CPIonID[(CPIonID['year'] >= 2003) & (CPIonID['year'] <= 2015)]

CPIData = CPIonYear[CPIonYear.period != 'M13']

CPIData = CPIData.drop('footnote_codes', 1)

MonthCol = pd.DataFrame([None for x in range(len(CPIData))])
MonthCol.columns = ['month'] 

CPIData = pd.concat([CPIData.reset_index(drop=True), MonthCol], axis=1)

CPIData.loc[CPIData.period == 'M01', 'month'] = 1
CPIData.loc[CPIData.period == 'M02', 'month'] = 2
CPIData.loc[CPIData.period == 'M03', 'month'] = 3
CPIData.loc[CPIData.period == 'M04', 'month'] = 4
CPIData.loc[CPIData.period == 'M05', 'month'] = 5
CPIData.loc[CPIData.period == 'M06', 'month'] = 6
CPIData.loc[CPIData.period == 'M07', 'month'] = 7
CPIData.loc[CPIData.period == 'M08', 'month'] = 8
CPIData.loc[CPIData.period == 'M09', 'month'] = 9
CPIData.loc[CPIData.period == 'M10', 'month'] = 10
CPIData.loc[CPIData.period == 'M11', 'month'] = 11
CPIData.loc[CPIData.period == 'M12', 'month'] = 12

RegionCol = pd.DataFrame([None for x in range(len(CPIData))])
RegionCol.columns = ['region'] 

CPIData = pd.concat([CPIData.reset_index(drop=True), RegionCol], axis=1)

CPIData.loc[CPIData.series_id == 'CUUR0100SA0', 'region'] = 'Northeast'
CPIData.loc[CPIData.series_id == 'CUUR0200SA0', 'region'] = 'Midwest'
CPIData.loc[CPIData.series_id == 'CUUR0300SA0', 'region'] = 'South'
CPIData.loc[CPIData.series_id == 'CUUR0400SA0', 'region'] = 'West'

CPIData = CPIData.sort_values(['series_id', 'year', 'month'])
CPIData = CPIData.reset_index(drop = True)

LagCPIData = CPIData.value.shift()
LagCPIData = pd.DataFrame(LagCPIData)
LagCPIData.columns = ['lagvalue']

CPIData = pd.concat([CPIData, LagCPIData], axis=1)

CPIData = CPIData[CPIData['year'] >= 2003]



CPIData.to_csv("habit-formation/UseData/CPI.csv", index=False)



