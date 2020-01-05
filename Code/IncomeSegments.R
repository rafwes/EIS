
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
print(length(unique(Trips4$household_code)))

# Income Segments
TripsLow <- filter(Trips4, household_income <= 13)
TripsMid <- filter(Trips4, household_income > 13 & household_income < 27)
TripsHigh <- filter(Trips4, household_income >= 27)

print("Number of Households - Income Segment")
print("Low Income")
print(length(unique(TripsLow$household_code)))
print("Middle Income")
print(length(unique(TripsMid$household_code)))
print("High Income")
print(length(unique(TripsHigh$household_code)))