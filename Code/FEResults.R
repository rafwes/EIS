

# cd "/extra/agalvao/eis_nielsen"

rm(list = ls())

library(zoo)
library(plm)
library(stargazer)
library(lubridate)
library(pastecs)
library(xtable)

########################################################################
########################################################################

Trips4 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
#Trips4$AvgAge <- (Trips4$male_head_age + Trips4$female_head_age)/2
# Young
#Trips4 <- Trips4[which(Trips4$AvgAge <= 5.5),]
# Old
#Trips4 <- Trips4[which(Trips4$AvgAge >= 6),]
# Low Income
#Trips4 <- Trips4[which(Trips4$household_income <= 19),]
# High Income
#Trips4 <- Trips4[which(Trips4$household_income >= 21),]

print(unique(cbind(Trips4$year, Trips4$month)))

Trips4_1a <- Trips4[which(Trips4$year == 2004 & Trips4$month == 2),]
Beg1 <- unique(Trips4_1a$monthR)
Trips4_1b <- Trips4[which(Trips4$year == 2006 & Trips4$month == 8),]
End1 <- unique(Trips4_1b$monthR)

Trips4_2a <- Trips4[which(Trips4$year == 2006 & Trips4$month == 9),]
Beg2 <- unique(Trips4_2a$monthR)
Trips4_2b <- Trips4[which(Trips4$year == 2008 & Trips4$month == 12),]
End2 <- unique(Trips4_2b$monthR)

Trips4_3a <- Trips4[which(Trips4$year == 2009 & Trips4$month == 1),]
Beg3 <- unique(Trips4_3a$monthR)
Trips4_3b <- Trips4[which(Trips4$year == 2014 & Trips4$month == 12),]
End3 <- unique(Trips4_3b$monthR)

print(Beg1)
print(End1)
print(Beg2)
print(End2)
print(Beg3)
print(End3)


########################################################################
########################################################################

#Trips4_1 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
Trips4_1 <- Trips4[which(Trips4$monthR >= Beg1 & Trips4$monthR <= End1),]
Trips4_1$household_code <- factor(Trips4_1$household_code)
Trips4_1$week <- factor(Trips4_1$week)
Trips4_1$month <- factor(Trips4_1$month)
Trips4_1$weekR <- factor(Trips4_1$weekR)
Trips4_1$monthR <- factor(Trips4_1$monthR)

print(nrow(Trips4_1))
print(unique(cbind(Trips4_1$year, Trips4_1$month)))

#########################################################################

Weekly4LagInst1 <- plm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4_1, model='within', index=c('household_code', 'weekR'))
summary(Weekly4LagInst1)
#save(Weekly4LagInst1, file="RDA/Weekly4LagInstT1_A.rda")

print("1")

#########################################################################

#Weekly4LagInstWITest1 <- plm(LogR ~ YInst + Lag2LogNomR + Lag2Inf, data=Trips4_1, model="within", index=c('household_code', 'weekR'))
#summary(Weekly4LagInstWITest1)
#save(Weekly4LagInstWITest1, file="RDA/Weekly4LagInstWITestT1_A.rda")

print("2")

#########################################################################
#########################################################################
#
#Trips4_2 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
Trips4_2 <- Trips4[which(Trips4$monthR >= Beg2 & Trips4$monthR <= End2),]
Trips4_2$household_code <- factor(Trips4_2$household_code)
Trips4_2$week <- factor(Trips4_2$week)
Trips4_2$month <- factor(Trips4_2$month)
Trips4_2$weekR <- factor(Trips4_2$weekR)
Trips4_2$monthR <- factor(Trips4_2$monthR)

print(nrow(Trips4_2))
print(unique(cbind(Trips4_2$year, Trips4_2$month)))

#########################################################################

Weekly4LagInst2 <- plm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4_2, model='within', index=c('household_code', 'weekR'))
summary(Weekly4LagInst2)
#save(Weekly4LagInst2, file="RDA/Weekly4LagInstT2_A.rda")

print("3")

#########################################################################

#Weekly4LagInstWITest2 <- plm(LogR ~ YInst + Lag2LogNomR + Lag2Inf, data=Trips4_2, model="within", index=c('household_code', 'weekR'))
#summary(Weekly4LagInstWITest2)
#save(Weekly4LagInstWITest2, file="RDA/Weekly4LagInstWITestT2_A.rda")

print("4")

#########################################################################
#########################################################################

#Trips4_3 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
Trips4_3 <- Trips4[which(Trips4$monthR >= Beg3 & Trips4$monthR <= End3),]
Trips4_3$household_code <- factor(Trips4_3$household_code)
Trips4_3$week <- factor(Trips4_3$week)
Trips4_3$month <- factor(Trips4_3$month)
Trips4_3$weekR <- factor(Trips4_3$weekR)
Trips4_3$monthR <- factor(Trips4_3$monthR)

print(nrow(Trips4_3))
print(unique(cbind(Trips4_3$year, Trips4_3$month)))

#########################################################################

Weekly4LagInst3 <- plm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4_3, model='within', index=c('household_code', 'weekR'))
summary(Weekly4LagInst3)
#save(Weekly4LagInst3, file="RDA/Weekly4LagInstT3_A.rda")

print("5")

#########################################################################

#Weekly4LagInstWITest3 <- plm(LogR ~ YInst + Lag2LogNomR + Lag2Inf, data=Trips4_3, model="within", index=c('household_code', 'weekR'))
#summary(Weekly4LagInstWITest3)
#save(Weekly4LagInstWITest3, file="RDA/Weekly4LagInstWITestT3_A.rda")

print("6")

#########################################################################
#########################################################################

FEResultsSeparated <- stargazer(Weekly4LagInst1, Weekly4LagInst2, Weekly4LagInst3, title="Estimates of the EIS", column.labels = c("2004-2006", "2007-2008", "2009-2014"), dep.var.labels = "$\\Delta c_{t+4}$", covariate.labels = "$r_{t+4}$", omit = "month", out="EIS/Output/FEResultsSeparated.tex", label="FEResultsSeparated", no.space=TRUE)

#########################################################################
#########################################################################

Trips4$household_code <- factor(Trips4$household_code)
Trips4$week <- factor(Trips4$week)
Trips4$month <- factor(Trips4$month)
Trips4$weekR <- factor(Trips4$weekR)
Trips4$monthR <- factor(Trips4$monthR)

print(nrow(Trips4))
print(unique(cbind(Trips4$year, Trips4$month)))

#########################################################################

Weekly4LagInstAll <- plm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4, model='within', index=c('household_code', 'weekR'))
summary(Weekly4LagInstAll)
#save(Weekly4LagInst3, file="RDA/Weekly4LagInstT3_A.rda")

print("7")

#########################################################################

#Weekly4LagInstWITest3 <- plm(LogR ~ YInst + Lag2LogNomR + Lag2Inf, data=Trips4_3, model="within", index=c('household_code', 'weekR'))
#summary(Weekly4LagInstWITest3)
#save(Weekly4LagInstWITest3, file="RDA/Weekly4LagInstWITestT3_A.rda")

print("8")

#########################################################################
#########################################################################

FEResultsAll <- stargazer(Weekly4LagInstAll, title="Estimates of the EIS", omit = "month", out="EIS/Output/FEResultsAll.tex", label="FEResultsAll", no.space=TRUE)




