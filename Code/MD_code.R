

# cd "/extra/agalvao/eis_nielsen"
setwd("/Users/lancecundy/Documents/Research/Nielsen/EIS")

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
library(quantreg)

########################################################################
########################################################################

source("EIS/Code/KaplanNew/gmmq.R")

Trips4 <- read.csv('UseData/SampleData.csv')
Trips4 <- Trips4[c('household_code', 'week', 'month', 'weekR', 'monthR', 'year', 'Y', 'LogR', 'YInst', 'Lag2LogNomR', 'Lag2Inf')]

household_count <- Trips4 %>%
  group_by(household_code) %>%
  summarise(n())


Trips_by_house <- Trips4 %>% split(.$household_code)


mods <- Trips_by_house %>% map(~ rq(Y ~ LogR, data = ., tau = seq(0.5,0.5,0.1)))
mods <- Trips_by_house %>% map(~ rq(Y ~ LogR, data = ., tau = seq(0.1,0.9,0.1)))








