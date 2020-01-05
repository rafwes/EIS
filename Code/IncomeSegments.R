
# cd "/extra/agalvao/eis_nielsen"

rm(list = ls())

library(zoo)
library(plm)
library(stargazer)
library(lubridate)
library(pastecs)
library(xtable)
library(pracma)
library(AER)
library(Matrix)
library(MASS)
library(tidyverse)

########################################################################
########################################################################

Trips4 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
Trips4 <- Trips4[c('household_code', 'week', 'month', 'weekR', 'monthR', 'year', 'Y', 'LogR', 'YInst', 'Lag2LogNomR', 'Lag2Inf', 'household_income')]

print("Number of Households - All")
print(nrow(unique(Trips4$household_code)))

# Income Segments
TripsLow <- filter(Trips4, household_income <= 13)
Trips4Mid <- filter(Trips4, household_income > 13 & household_income < 27)
Trips4High <- filter(Trips4, household_income >= 27)

print("Number of Households - Income Segment")
print("Low Income")
print(nrow(unique(TripsLow$household_code)))
print("Middle Income")
print(nrow(unique(TripsMid$household_code)))
print("High Income")
print(nrow(unique(TripsHigh$household_code)))