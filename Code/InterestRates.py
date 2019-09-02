# cd 'Documents/UIowa/Research/Nielsen/Habit Formation'

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

CPIData = pd.read_csv("habit-formation/UseData/CPI.csv", sep=",")
TBillData = pd.read_csv("habit-formation/UseData/TBill.csv", sep=",")

IRData = pd.merge(CPIData, TBillData, how='left', on=['year', 'month'])

IRData = IRData.sort_values(['year', 'month', 'series_id'])
IRData = IRData.reset_index(drop = True)

#IRDataNE = IRData[(IRData['region'] == 'Northeast')]
#IRDataNE = IRDataNE.reset_index(drop=True)
#CPIBaseNE = IRDataNE.loc[0,'value']

#IRDataMW = IRData[(IRData['region'] == 'Midwest')]
#IRDataMW = IRDataMW.reset_index(drop=True)
#CPIBaseMW = IRDataMW.loc[0,'value']

#IRDataS = IRData[(IRData['region'] == 'South')]
#IRDataS = IRDataS.reset_index(drop=True)
#CPIBaseS = IRDataS.loc[0,'value']

#IRDataW = IRData[(IRData['region'] == 'West')]
#IRDataW = IRDataW.reset_index(drop=True)
#CPIBaseW = IRDataW.loc[0,'value']

#def AddBase(row):
#    if (row['region'] == 'Northeast'): 
#        return CPIBaseNE
#    elif (row['region'] == 'Midwest'):
#        return CPIBaseMW
#    elif (row['region'] == 'South'):
#        return CPIBaseS
#    elif (row['region'] == 'West'):
#        return CPIBaseW
#    else:
#        return
        
#IRData['CPIBase'] = IRData.apply(AddBase, axis=1)

#IRData['r'] = IRData['TB3MS'] * (IRData['value'] / IRData['CPIBase'])
IRData['r'] = IRData['TB4WK'] - ((IRData['value'] / IRData['lagvalue']) - 1)

IRData.to_csv("habit-formation/UseData/IRData.csv", index=False)

