

# cd "/extra/agalvao/eis_nielsen"

rm(list = ls())

library(zoo)
library(plm)
library(stargazer)
library(lubridate)

print("1")

#Trips <- read.csv('UseData/GroceryTripsSmall.csv')
Trips <- read.csv('Datasets/GroceryTrips.csv')

Trips$projection_factor_magnet <- ifelse(is.na(Trips$projection_factor_magnet), 0, Trips$projection_factor_magnet)


MonthlyIR <- read.csv('EIS/UseData/MonthlyTB4WK.csv')

BadTrips <- which(Trips$panel_year != Trips$year)
Trips <- Trips[-BadTrips,]

TripCols <- c('household_code', 'purchase_date', 'projection_factor_magnet', 'month', 'year', 'total_spent', 'r', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'value', 'lagvalue', 'region')
Trips <- Trips[TripCols]
Trips$marital_status <- factor(Trips$marital_status)
Trips$household_income <- factor(Trips$household_income)
Trips$male_head_age <- factor(Trips$male_head_age)
Trips$female_head_age <- factor(Trips$female_head_age)
Trips$male_head_education <- factor(Trips$male_head_education)
Trips$female_head_education <- factor(Trips$female_head_education)

colnames(Trips)[colnames(Trips)=="r"] <- "monthlyr"
colnames(Trips)[colnames(Trips)=="value"] <- "CPI"
colnames(Trips)[colnames(Trips)=="lagvalue"] <- "LagCPI"

Trips$Inflation <- Trips$CPI / Trips$LagCPI

print("2")

###

ZeroTrips <- which(Trips$total_spent == 0)
Trips <- Trips[-ZeroTrips,]

####

MaxYear <- max(Trips$year)
MaxCPIData <- Trips[which(Trips$year==MaxYear & Trips$month == 1),]
MaxCPISmall <- MaxCPIData[c("region", "CPI")] 

UniqueMaxCPISmall <- unique(MaxCPISmall)
colnames(UniqueMaxCPISmall)[colnames(UniqueMaxCPISmall) == 'CPI'] <- 'BaseCPI'

Trips <- merge(Trips, UniqueMaxCPISmall, by=c('region'))

Trips$real_total_spent <- Trips$total_spent * (Trips$BaseCPI / Trips$CPI)



print("2.5")

###


######

# Middle 90%

#Trips <- na.omit(Trips)

#SumAmtMonth <- aggregate(Trips$real_total_spent, by=list(Trips$household_code, Trips$month), FUN=sum)
#colnames(SumAmtMonth) <- c('household_code', 'month', 'SumAmtMonth')

#MaxAmt <- aggregate(SumAmtMonth$SumAmtMonth, by=list(SumAmtMonth$household_code), FUN=max)
#colnames(MaxAmt) <- c('household_code', 'MaxAmt')

#Amt95 <- quantile(MaxAmt$MaxAmt, probs = 0.95)
#Amt5 <- quantile(MaxAmt$MaxAmt, probs = 0.05)

#MaxAmt$Normal <- ifelse(MaxAmt$MaxAmt > Amt95 | MaxAmt$MaxAmt < Amt5, NA, 1)
#MaxAmt <- na.omit(MaxAmt)
#TripsAdj <- merge(Trips, MaxAmt, by='household_code')
#Trips <- na.omit(TripsAdj)

#####


#Trips['week'] <- isoweek(Trips$purchase_date)


MonthlyIR['month'] <- month(MonthlyIR$DATE)
MonthlyIR['year'] <- year(MonthlyIR$DATE)



Trips <- merge(Trips, MonthlyIR, by=c('year', 'month'))
Trips <- na.omit(Trips)









#BadWeeks <- which(Trips$week == 52 | Trips$week == 53 | Trips$week == 1)
#Trips <- Trips[-BadWeeks,]

Trips['monthR'] <- Trips$month
Trips['monthR'] <- ifelse(Trips$year == 2004, Trips$month + 12, Trips$monthR)
Trips['monthR'] <- ifelse(Trips$year == 2005, Trips$month + 12*2, Trips$monthR) 
Trips['monthR'] <- ifelse(Trips$year == 2006, Trips$month + 12*3, Trips$monthR) 
Trips['monthR'] <- ifelse(Trips$year == 2007, Trips$month + 12*4, Trips$monthR) 
Trips['monthR'] <- ifelse(Trips$year == 2008, Trips$month + 12*5, Trips$monthR) 
Trips['monthR'] <- ifelse(Trips$year == 2009, Trips$month + 12*6, Trips$monthR) 
Trips['monthR'] <- ifelse(Trips$year == 2010, Trips$month + 12*7, Trips$monthR)
Trips['monthR'] <- ifelse(Trips$year == 2011, Trips$month + 12*8, Trips$monthR) 
Trips['monthR'] <- ifelse(Trips$year == 2012, Trips$month + 12*9, Trips$monthR)
Trips['monthR'] <- ifelse(Trips$year == 2013, Trips$month + 12*10, Trips$monthR)
Trips['monthR'] <- ifelse(Trips$year == 2014, Trips$month + 12*11, Trips$monthR)
Trips['monthR'] <- ifelse(Trips$year == 2015, Trips$month + 12*12, Trips$monthR)
Trips['monthR'] <- ifelse(Trips$year == 2016, Trips$month + 12*13, Trips$monthR)  

Trips$month <- factor(Trips$month)

#Trips[which(Trips$household_code == 2000368 & Trips$monthR == 4),]

TripsCPI <- aggregate(Trips$CPI, by=list(Trips$household_code, Trips$monthR), FUN=mean)
colnames(TripsCPI) <- c('household_code', 'monthR', 'CPI')
TripsLagCPI <- aggregate(Trips$LagCPI, by=list(Trips$household_code, Trips$monthR), FUN=mean)
colnames(TripsLagCPI) <- c('household_code', 'monthR', 'LagCPI')

Trips$CPI <- NULL
Trips$LagCPI <- NULL
Trips$Inflation <- NULL

Trips <- merge(Trips, TripsCPI, by=c('household_code', 'monthR'))
Trips <- merge(Trips, TripsLagCPI, by=c('household_code', 'monthR'))
Trips <- merge(Trips, TripsInflation, by=c('household_code', 'monthR'))


Trips$r <- (Trips$TB4WK * 0.01) - ((Trips$CPI / Trips$LagCPI) -1 )



#TripsMonth <- aggregate(Trips$month, by = list(Trips$household_code, Trips$weekR), FUN=min)
#colnames(TripsMonth) <- c('household_code', 'weekR', 'month')


Trips <- Trips[order(Trips$household_code, Trips$monthR),] 
rownames(Trips) <- 1:nrow(Trips)





Trips <- aggregate(Trips$total_spent, by=list(Trips$household_code, Trips$monthR, Trips$month, Trips$year, Trips$projection_factor_magnet, Trips$r, Trips$household_income, Trips$household_size, Trips$marital_status, Trips$male_head_age, Trips$female_head_age, Trips$male_head_education, Trips$female_head_education, Trips$TB4WK, Trips$Inflation), FUN=sum)
colnames(Trips) <- c('household_code', 'monthR', 'month', 'year', 'projection_factor_magnet', 'r',  'household_income', 'household_size', 'marital_status', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'Nomr', 'Inflation', 'total_spent')


#Trips <- merge(Trips, TripsMonth, by=c('household_code', 'weekR'))
#Trips$month <- factor(Trips$month)


Trips <- Trips[order(Trips$household_code, Trips$monthR),] 
rownames(Trips) <- 1:nrow(Trips)



Trips['R'] <- 1 + Trips$r
Trips['LogR'] <- log(Trips$R)
Trips['NomR'] <- 1 + (Trips$Nomr * 0.01)
Trips['LogNomR'] <- log(Trips$NomR)



Trips['LogC'] <- log(Trips$total_spent)

Trips <- na.omit(Trips)


#Trips[which(Trips$household_code==2000351 & Trips$weekR == 387),]


#write.csv(Trips, "Datasets/TripsMid.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/TripsMid90.csv", row.names=FALSE)

Trips <- Trips[order(Trips$household_code, Trips$monthR),] 
rownames(Trips) <- 1:nrow(Trips)


ChangeLogC <- diff(Trips$LogC)
ChangeLogC <- data.frame(ChangeLogC)
ChangeLogC <- rbind('NA', ChangeLogC)
ChangeLogR <- diff(Trips$LogR)
ChangeLogR <- data.frame(ChangeLogR)
ChangeLogR <- rbind('NA', ChangeLogR)
ChangeLogNomR <- diff(Trips$LogNomR)
ChangeLogNomR <- data.frame(ChangeLogNomR)
ChangeLogNomR <- rbind('NA', ChangeLogNomR)
ChangeInf <- diff(Trips$Inflation)
ChangeInf <- data.frame(ChangeInf)
ChangeInf <- rbind('NA', ChangeInf)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, ChangeLogC, ChangeLogR, ChangeLogNomR, ChangeInf)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$ChangeLogC <- as.numeric(as.character(Trips$ChangeLogC))
Trips$ChangeLogR <- as.numeric(as.character(Trips$ChangeLogR))
Trips$ChangeLogNomR <- as.numeric(as.character(Trips$ChangeLogNomR))
Trips$ChangeInf <- as.numeric(as.character(Trips$ChangeInf))

Trips$LagLogC <- Trips$LogC - Trips$ChangeLogC
Trips$LagLogR <- Trips$LogR - Trips$ChangeLogR
Trips$LagLogNomR <- Trips$LogNomR - Trips$ChangeLogNomR
Trips$LagInf <- Trips$Inflation - Trips$ChangeInf

TripCols <- c('household_code', 'monthR', 'month', 'year', 'projection_factor_magnet', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$monthR),] 
rownames(Trips) <- 1:nrow(Trips)


Change1LogC <- diff(Trips$LagLogC)
Change1LogC <- data.frame(Change1LogC)
Change1LogC <- rbind('NA', Change1LogC)
Change1LogR <- diff(Trips$LagLogR)
Change1LogR <- data.frame(Change1LogR)
Change1LogR <- rbind('NA', Change1LogR)
Change1LogNomR <- diff(Trips$LagLogNomR)
Change1LogNomR <- data.frame(Change1LogNomR)
Change1LogNomR <- rbind('NA', Change1LogNomR)
Change1Inf <- diff(Trips$LagInf)
Change1Inf <- data.frame(Change1Inf)
Change1Inf <- rbind('NA', Change1Inf)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, Change1LogC, Change1LogR, Change1LogNomR, Change1Inf)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$Change1LogC <- as.numeric(as.character(Trips$Change1LogC))
Trips$Change1LogR <- as.numeric(as.character(Trips$Change1LogR))
Trips$Change1LogNomR <- as.numeric(as.character(Trips$Change1LogNomR))
Trips$Change1Inf <- as.numeric(as.character(Trips$Change1Inf))

Trips$Lag2LogC <- Trips$LagLogC - Trips$Change1LogC
Trips$Lag2LogR <- Trips$LagLogR - Trips$Change1LogR
Trips$Lag2LogNomR <- Trips$LagLogNomR - Trips$Change1LogNomR
Trips$Lag2Inf <- Trips$LagInf - Trips$Change1Inf

TripCols <- c('household_code', 'monthR', 'month', 'year', 'projection_factor_magnet', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'Change1LogC', 'Change1LogR', 'Lag2LogC', 'Lag2LogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'Change1LogNomR', 'Lag2LogNomR', 'Change1Inf', 'Lag2Inf')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$monthR),] 
rownames(Trips) <- 1:nrow(Trips)


Change2LogC <- diff(Trips$Lag2LogC)
Change2LogC <- data.frame(Change2LogC)
Change2LogC <- rbind('NA', Change2LogC)
Change2LogR <- diff(Trips$Lag2LogR)
Change2LogR <- data.frame(Change2LogR)
Change2LogR <- rbind('NA', Change2LogR)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, Change2LogC, Change2LogR)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$Change2LogC <- as.numeric(as.character(Trips$Change2LogC))
Trips$Change2LogR <- as.numeric(as.character(Trips$Change2LogR))


TripCols <- c('household_code', 'monthR', 'month', 'year', 'projection_factor_magnet', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'Change1LogC', 'Change1LogR', 'Lag2LogC', 'Lag2LogR', 'Change2LogC', 'Change2LogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'Change1LogNomR', 'Lag2LogNomR', 'Change1Inf', 'Lag2Inf')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


#x <- cbind(Trips$household_code, Trips$monthR)
#duplicated(x)
#Trips[39:41,]
#Trips[which(Trips$household_code == 2000368 & Trips$monthR == 4),]

Trips <- Trips[order(Trips$household_code, Trips$monthR),] 
rownames(Trips) <- 1:nrow(Trips)

#write.csv(Trips, "Datasets/TripsFinal.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/TripsFinal90.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/TripsChangesMonthlyIR.csv", row.names=FALSE)
write.csv(Trips, "Datasets/RealTripsChangesMonthlyIR.csv", row.names=FALSE)

#TripsChangesMonthlyIRSmall <- Trips[1:10000,]
#write.csv(TripsChangesMonthlyIRSmall, "habit-formation/UseData/TripsChangesMonthlyIRAllSmall.csv", row.names=FALSE)














