

# cd "Documents/UIowa/Research/Nielsen/Habit Formation"

rm(list = ls())

library(zoo)
library(plm)
library(stargazer)
library(lubridate)

print("1")

#Trips <- read.csv('UseData/GroceryTripsSmall.csv')
Trips <- read.csv('Datasets/GroceryTrips.csv')

Trips$projection_factor_magnet <- ifelse(is.na(Trips$projection_factor_magnet), 0, Trips$projection_factor_magnet)

#WeeklyIR <- read.csv('UseData/WTB4WK.csv')
WeeklyIR <- read.csv('habit-formation/UseData/WTB4WK.csv')

BadTrips <- which(Trips$panel_year != Trips$year)
Trips <- Trips[-BadTrips,]

TripCols <- c('household_code', 'projection_factor_magnet', 'purchase_date', 'month', 'year', 'total_spent', 'r', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'value', 'lagvalue', 'region')
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


Trips['week'] <- isoweek(Trips$purchase_date)


WeeklyIR['week'] <- isoweek(WeeklyIR$DATE)
WeeklyIR['year'] <- year(WeeklyIR$DATE)



Trips <- merge(Trips, WeeklyIR, by=c('year', 'week'))
Trips <- na.omit(Trips)






BadWeeks <- which(Trips$week == 52 | Trips$week == 53 | Trips$week == 1)
Trips <- Trips[-BadWeeks,]

Trips['weekR'] <- Trips$week
Trips['weekR'] <- ifelse(Trips$year == 2004, Trips$week + 52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2005, Trips$week + 52+53, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2006, Trips$week + 52+53+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2007, Trips$week + 52+53+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2008, Trips$week + 52+53+52+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2009, Trips$week + 52+53+52+52+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2010, Trips$week + 52+53+52+52+52+52+53, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2011, Trips$week + 52+53+52+52+52+52+53+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2012, Trips$week + 52+53+52+52+52+52+53+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2013, Trips$week + 52+53+52+52+52+52+53+52+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2014, Trips$week + 52+53+52+52+52+52+53+52+52+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2015, Trips$week + 52+53+52+52+52+52+53+52+52+52+52+52, Trips$weekR)
Trips['weekR'] <- ifelse(Trips$year == 2016, Trips$week + 52+53+52+52+52+52+53+52+52+52+52+52+53, Trips$weekR)

Trips$week <- factor(Trips$week)

print("3")

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

#Trips$month <- factor(Trips$month)

Trips$year <- NULL

print("4")

TripsCPI <- aggregate(Trips$CPI, by=list(Trips$household_code, Trips$weekR), FUN=mean)
colnames(TripsCPI) <- c('household_code', 'weekR', 'CPI')
TripsLagCPI <- aggregate(Trips$LagCPI, by=list(Trips$household_code, Trips$weekR), FUN=mean)
colnames(TripsLagCPI) <- c('household_code', 'weekR', 'LagCPI')
TripsInflation <- aggregate(Trips$Inflation, by=list(Trips$household_code, Trips$weekR), FUN=mean)
colnames(TripsInflation) <- c('household_code', 'weekR', 'Inflation')

Trips$CPI <- NULL
Trips$LagCPI <- NULL
Trips$Inflation <- NULL

Trips <- merge(Trips, TripsCPI, by=c('household_code', 'weekR'))
Trips <- merge(Trips, TripsLagCPI, by=c('household_code', 'weekR'))
Trips <- merge(Trips, TripsInflation, by=c('household_code', 'weekR'))


Trips$r <- (Trips$WTB4WK * 0.01) - ((Trips$CPI / Trips$LagCPI) -1 )

print("4.5")

TripsMonth <- aggregate(Trips$month, by = list(Trips$household_code, Trips$weekR), FUN=min)
colnames(TripsMonth) <- c('household_code', 'weekR', 'month')

print("5")

TripsMonthR <- aggregate(Trips$monthR, by = list(Trips$household_code, Trips$weekR), FUN=min)
colnames(TripsMonthR) <- c('household_code', 'weekR', 'monthR')

print("6")

head(Trips)


Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)


#Trips[which(Trips$household_code==2000351 & Trips$weekR == 387),]


Trips <- aggregate(Trips$real_total_spent, by=list(Trips$household_code, Trips$projection_factor_magnet, Trips$weekR, Trips$week, Trips$r, Trips$household_income, Trips$household_size, Trips$marital_status, Trips$male_head_age, Trips$female_head_age, Trips$male_head_education, Trips$female_head_education, Trips$WTB4WK, Trips$Inflation), FUN=sum)
colnames(Trips) <- c('household_code', 'projection_factor_magnet', 'weekR', 'week', 'r',  'household_income', 'household_size', 'marital_status', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'Nomr', 'Inflation', 'real_total_spent')


Trips <- merge(Trips, TripsMonth, by=c('household_code', 'weekR'))
Trips$month <- factor(Trips$month)

print("7")

Trips <- merge(Trips, TripsMonthR, by=c('household_code', 'weekR'))
#Trips$monthR <- factor(Trips$monthR)

print("8")

Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)



Trips['R'] <- 1 + Trips$r
Trips['LogR'] <- log(Trips$R)
Trips['NomR'] <- 1 + (Trips$Nomr * 0.01)
Trips['LogNomR'] <- log(Trips$NomR)



Trips['LogC'] <- log(Trips$real_total_spent)

Trips <- na.omit(Trips)

# Add years back in
Trips['year'] <- 2003
Trips['year'] <- ifelse(Trips$monthR > 12*1 & Trips$monthR <= 12*2, Trips$year + 1, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*2 & Trips$monthR <= 12*3, Trips$year + 2, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*3 & Trips$monthR <= 12*4, Trips$year + 3, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*4 & Trips$monthR <= 12*5, Trips$year + 4, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*5 & Trips$monthR <= 12*6, Trips$year + 5, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*6 & Trips$monthR <= 12*7, Trips$year + 6, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*7 & Trips$monthR <= 12*8, Trips$year + 7, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*8 & Trips$monthR <= 12*9, Trips$year + 8, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*9 & Trips$monthR <= 12*10, Trips$year + 9, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*10 & Trips$monthR <= 12*11, Trips$year + 10, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*11 & Trips$monthR <= 12*12, Trips$year + 11, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*12 & Trips$monthR <= 12*13, Trips$year + 12, Trips$year)
Trips['year'] <- ifelse(Trips$monthR > 12*13 & Trips$monthR <= 12*14, Trips$year + 13, Trips$year)

Trips$year <- factor(Trips$year)
Trips$monthR <- factor(Trips$monthR)




#Trips[which(Trips$household_code==2000351 & Trips$weekR == 387),]


#write.csv(Trips, "Datasets/TripsMid.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/TripsMid90.csv", row.names=FALSE)

Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)

print("9")


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
ChangeFS <- diff(Trips$household_size)
ChangeFS <- data.frame(ChangeFS)
ChangeFS <- rbind('NA', ChangeFS)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, ChangeLogC, ChangeLogR, ChangeLogNomR, ChangeInf, ChangeFS)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$ChangeLogC <- as.numeric(as.character(Trips$ChangeLogC))
Trips$ChangeLogR <- as.numeric(as.character(Trips$ChangeLogR))
Trips$ChangeLogNomR <- as.numeric(as.character(Trips$ChangeLogNomR))
Trips$ChangeInf <- as.numeric(as.character(Trips$ChangeInf))
Trips$ChangeFS <- as.numeric(as.character(Trips$ChangeFS))

Trips$LagLogC <- Trips$LogC - Trips$ChangeLogC
Trips$LagLogR <- Trips$LogR - Trips$ChangeLogR
Trips$LagLogNomR <- Trips$LogNomR - Trips$ChangeLogNomR
Trips$LagInf <- Trips$Inflation - Trips$ChangeInf
Trips$LagFS <- Trips$household_size - Trips$ChangeFS

TripCols <- c('household_code', 'projection_factor_magnet', 'weekR', 'week', 'month', 'monthR', 'year', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'real_total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'ChangeFS', 'LagFS')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)

print("10")


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
Change1FS <- diff(Trips$LagFS)
Change1FS <- data.frame(Change1FS)
Change1FS <- rbind('NA', Change1FS)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, Change1LogC, Change1LogR, Change1LogNomR, Change1Inf, Change1FS)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$Change1LogC <- as.numeric(as.character(Trips$Change1LogC))
Trips$Change1LogR <- as.numeric(as.character(Trips$Change1LogR))
Trips$Change1LogNomR <- as.numeric(as.character(Trips$Change1LogNomR))
Trips$Change1Inf <- as.numeric(as.character(Trips$Change1Inf))
Trips$Change1FS <- as.numeric(as.character(Trips$Change1FS))

Trips$Lag2LogC <- Trips$LagLogC - Trips$Change1LogC
Trips$Lag2LogR <- Trips$LagLogR - Trips$Change1LogR
Trips$Lag2LogNomR <- Trips$LagLogNomR - Trips$Change1LogNomR
Trips$Lag2Inf <- Trips$LagInf - Trips$Change1Inf
Trips$Lag2FS <- Trips$LagFS - Trips$Change1FS

TripCols <- c('household_code', 'projection_factor_magnet', 'weekR', 'week', 'month', 'monthR', 'year', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'real_total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'Change1LogC', 'Change1LogR', 'Lag2LogC', 'Lag2LogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'Change1LogNomR', 'Lag2LogNomR', 'Change1Inf', 'Lag2Inf', 'ChangeFS', 'LagFS', 'Change1FS', 'Lag2FS')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)

print("11")


Change2LogC <- diff(Trips$Lag2LogC)
Change2LogC <- data.frame(Change2LogC)
Change2LogC <- rbind('NA', Change2LogC)
Change2LogR <- diff(Trips$Lag2LogR)
Change2LogR <- data.frame(Change2LogR)
Change2LogR <- rbind('NA', Change2LogR)
Change2LogNomR <- diff(Trips$Lag2LogNomR)
Change2LogNomR <- data.frame(Change2LogNomR)
Change2LogNomR <- rbind('NA', Change2LogNomR)
Change2Inf <- diff(Trips$Lag2Inf)
Change2Inf <- data.frame(Change2Inf)
Change2Inf <- rbind('NA', Change2Inf)
Change2FS <- diff(Trips$Lag2FS)
Change2FS <- data.frame(Change2FS)
Change2FS <- rbind('NA', Change2FS)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, Change2LogC, Change2LogR, Change2LogNomR, Change2Inf, Change2FS)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$Change2LogC <- as.numeric(as.character(Trips$Change2LogC))
Trips$Change2LogR <- as.numeric(as.character(Trips$Change2LogR))
Trips$Change2LogNomR <- as.numeric(as.character(Trips$Change2LogNomR))
Trips$Change2Inf <- as.numeric(as.character(Trips$Change2Inf))
Trips$Change2FS <- as.numeric(as.character(Trips$Change2FS))

Trips$Lag3LogC <- Trips$Lag2LogC - Trips$Change2LogC
Trips$Lag3LogR <- Trips$Lag2LogR - Trips$Change2LogR
Trips$Lag3LogNomR <- Trips$Lag2LogNomR - Trips$Change2LogNomR
Trips$Lag3Inf <- Trips$Lag2Inf - Trips$Change2Inf
Trips$Lag3FS <- Trips$Lag2FS - Trips$Change2FS


TripCols <- c('household_code', 'projection_factor_magnet', 'weekR', 'week', 'month', 'monthR', 'year', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'real_total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'Change1LogC', 'Change1LogR', 'Lag2LogC', 'Lag2LogR', 'Change2LogC', 'Change2LogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'Change1LogNomR', 'Lag2LogNomR', 'Change1Inf', 'Lag2Inf', 'Change2LogNomR', 'Change2Inf', 'Lag3LogC', 'Lag3LogR', 'Lag3LogNomR', 'Lag3Inf', 'ChangeFS', 'LagFS', 'Change1FS', 'Lag2FS', 'Change2FS', 'Lag3FS')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)

print("12")



Change3LogC <- diff(Trips$Lag3LogC)
Change3LogC <- data.frame(Change3LogC)
Change3LogC <- rbind('NA', Change3LogC)
Change3LogR <- diff(Trips$Lag3LogR)
Change3LogR <- data.frame(Change3LogR)
Change3LogR <- rbind('NA', Change3LogR)
Change3LogNomR <- diff(Trips$Lag3LogNomR)
Change3LogNomR <- data.frame(Change3LogNomR)
Change3LogNomR <- rbind('NA', Change3LogNomR)
Change3Inf <- diff(Trips$Lag3Inf)
Change3Inf <- data.frame(Change3Inf)
Change3Inf <- rbind('NA', Change3Inf)
Change3FS <- diff(Trips$Lag3FS)
Change3FS <- data.frame(Change3FS)
Change3FS <- rbind('NA', Change3FS)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, Change3LogC, Change3LogR, Change3LogNomR, Change3Inf, Change3FS)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$Change3LogC <- as.numeric(as.character(Trips$Change3LogC))
Trips$Change3LogR <- as.numeric(as.character(Trips$Change3LogR))
Trips$Change3LogNomR <- as.numeric(as.character(Trips$Change3LogNomR))
Trips$Change3Inf <- as.numeric(as.character(Trips$Change3Inf))
Trips$Change3FS <- as.numeric(as.character(Trips$Change3FS))

Trips$Lag4LogC <- Trips$Lag3LogC - Trips$Change3LogC
Trips$Lag4LogR <- Trips$Lag3LogR - Trips$Change3LogR
Trips$Lag4LogNomR <- Trips$Lag3LogNomR - Trips$Change3LogNomR
Trips$Lag4Inf <- Trips$Lag3Inf - Trips$Change3Inf
Trips$Lag4FS <- Trips$Lag3FS - Trips$Change3FS


TripCols <- c('household_code', 'projection_factor_magnet', 'weekR', 'week', 'month', 'monthR', 'year', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'real_total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'Change1LogC', 'Change1LogR', 'Lag2LogC', 'Lag2LogR', 'Change2LogC', 'Change2LogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'Change1LogNomR', 'Lag2LogNomR', 'Change1Inf', 'Lag2Inf', 'Change2LogNomR', 'Change2Inf', 'Lag3LogC', 'Lag3LogR', 'Lag3LogNomR', 'Lag3Inf', 'Change3LogC', 'Change3LogR', 'Change3LogNomR', 'Change3Inf', 'Lag4LogC', 'Lag4LogR', 'Lag4LogNomR', 'Lag4Inf', 'ChangeFS', 'LagFS', 'Change1FS', 'Lag2FS', 'Change2FS', 'Lag3FS', 'Change3FS', 'Lag4FS')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)

print("13")



Change4LogC <- diff(Trips$Lag4LogC)
Change4LogC <- data.frame(Change4LogC)
Change4LogC <- rbind('NA', Change4LogC)
Change4LogR <- diff(Trips$Lag4LogR)
Change4LogR <- data.frame(Change4LogR)
Change4LogR <- rbind('NA', Change4LogR)
Change4LogNomR <- diff(Trips$Lag4LogNomR)
Change4LogNomR <- data.frame(Change4LogNomR)
Change4LogNomR <- rbind('NA', Change4LogNomR)
Change4Inf <- diff(Trips$Lag4Inf)
Change4Inf <- data.frame(Change4Inf)
Change4Inf <- rbind('NA', Change4Inf)
Change4FS <- diff(Trips$Lag4FS)
Change4FS <- data.frame(Change4FS)
Change4FS <- rbind('NA', Change4FS)
HC <- zoo(Trips$household_code)
LagHC <- lag(HC, -1, na.pad=TRUE)
LagHC <- data.frame(LagHC)
Trips <- cbind(Trips, LagHC, Change4LogC, Change4LogR, Change4LogNomR, Change4Inf, Change4FS)

Trips$LagHC <- ifelse(Trips$household_code != Trips$LagHC, NA, Trips$household_code)

Trips <- na.omit(Trips)

Trips$Change4LogC <- as.numeric(as.character(Trips$Change4LogC))
Trips$Change4LogR <- as.numeric(as.character(Trips$Change4LogR))
Trips$Change4LogNomR <- as.numeric(as.character(Trips$Change4LogNomR))
Trips$Change4Inf <- as.numeric(as.character(Trips$Change4Inf))
Trips$Change4FS <- as.numeric(as.character(Trips$Change4FS))

Trips$Lag5LogC <- Trips$Lag4LogC - Trips$Change4LogC
Trips$Lag5LogR <- Trips$Lag4LogR - Trips$Change4LogR
Trips$Lag5LogNomR <- Trips$Lag4LogNomR - Trips$Change4LogNomR
Trips$Lag5Inf <- Trips$Lag4Inf - Trips$Change4Inf
Trips$Lag5FS <- Trips$Lag4FS - Trips$Change4FS


TripCols <- c('household_code', 'projection_factor_magnet', 'weekR', 'week', 'month', 'monthR', 'year', 'household_size', 'marital_status', 'household_income', 'male_head_age', 'female_head_age', 'male_head_education', 'female_head_education', 'real_total_spent', 'LogC', 'ChangeLogC', 'LogR', 'ChangeLogR', 'LagLogC', 'LagLogR', 'Change1LogC', 'Change1LogR', 'Lag2LogC', 'Lag2LogR', 'Change2LogC', 'Change2LogR', 'NomR', 'LogNomR', 'ChangeLogNomR', 'LagLogNomR', 'ChangeInf', 'LagInf', 'Change1LogNomR', 'Lag2LogNomR', 'Change1Inf', 'Lag2Inf', 'Change2LogNomR', 'Change2Inf', 'Lag3LogC', 'Lag3LogR', 'Lag3LogNomR', 'Lag3Inf', 'Change3LogC', 'Change3LogR', 'Change3LogNomR', 'Change3Inf', 'Lag4LogC', 'Lag4LogR', 'Lag4LogNomR', 'Lag4Inf', 'Change4LogC', 'Change4LogR', 'Change4LogNomR', 'Change4Inf', 'Lag5LogC', 'Lag5LogR', 'Lag5LogNomR', 'Lag5Inf', 'ChangeFS', 'LagFS', 'Change1FS', 'Lag2FS', 'Change2FS', 'Lag3FS', 'Change3FS', 'Lag4FS', 'Change4FS', 'Lag5FS')
Trips <- Trips[TripCols]
rownames(Trips) <- 1:nrow(Trips)


Trips <- Trips[order(Trips$household_code, Trips$weekR),] 
rownames(Trips) <- 1:nrow(Trips)

print("14")

Trips$Y <- Trips$LogC - Trips$Lag4LogC
Trips$YInst <- Trips$LagLogC - Trips$Lag5LogC
Trips$FSChange <- Trips$household_size - Trips$Lag4FS

Trips$Age <- apply(cbind(as.numeric(as.character(Trips$male_head_age)), as.numeric(as.character(Trips$female_head_age))), 1, max)
Trips$Edu <- apply(cbind(as.numeric(as.character(Trips$male_head_education)), as.numeric(as.character(Trips$female_head_education))), 1, max)

Trips$Age <- factor(Trips$Age)
Trips$Edu <- factor(Trips$Edu)


print("15")

head(Trips)

nrow(Trips)


#write.csv(Trips, "Datasets/TripsChangesWeeklyIR.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/TripsChangesWeeklyIR90.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/RealTripsChangesWeeklyIR.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/RealTripsChangesWeeklyIR90.csv", row.names=FALSE)
#write.csv(Trips, "Datasets/RealTripsChangesWeeklyIR90Time4.csv", row.names=FALSE)
write.csv(Trips, "Datasets/RealTripsChangesWeeklyIRAllTime4.csv", row.names=FALSE)

#RealTripsChangesWeeklyIRSmall <- Trips[1:10000,]
#write.csv(RealTripsChangesWeeklyIRSmall, "habit-formation/UseData/RealTripsChangesWeeklyIRSmall.csv", row.names=FALSE)














