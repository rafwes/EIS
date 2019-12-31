

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

# MyJob.po8036441 P3
#MyJob.po8039349 P1

########################################################################
########################################################################

Trips4 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
Trips4 <- Trips4[c('household_code', 'week', 'month', 'weekR', 'monthR', 'year', 'Y', 'LogR', 'YInst', 'Lag2LogNomR', 'Lag2Inf', 'household_income')]
#Trips4 <- filter(Trips4, household_income > 13)

#print(unique(cbind(Trips4$year, Trips4$month)))

Trips4_1a <- Trips4[which(Trips4$year == 2004 & Trips4$month == 2),]
Beg1 <- unique(Trips4_1a$monthR)
#Trips4_1b <- Trips4[which(Trips4$year == 2006 & Trips4$month == 8),]
#End1 <- unique(Trips4_1b$monthR)

#Trips4_2a <- Trips4[which(Trips4$year == 2006 & Trips4$month == 9),]
#Beg2 <- unique(Trips4_2a$monthR)
#Trips4_2b <- Trips4[which(Trips4$year == 2008 & Trips4$month == 12),]
#End2 <- unique(Trips4_2b$monthR)

#Trips4_3a <- Trips4[which(Trips4$year == 2009 & Trips4$month == 1),]
#Beg3 <- unique(Trips4_3a$monthR)
Trips4_3b <- Trips4[which(Trips4$year == 2014 & Trips4$month == 12),]
End3 <- unique(Trips4_3b$monthR)

print(Beg1)
#print(End1)
#print(Beg2)
#print(End2)
#print(Beg3)
print(End3)


########################################################################
########################################################################


Trips4_1 <- Trips4[which(Trips4$monthR >= Beg1 & Trips4$monthR <= End3),]
Trips4_1$household_code <- factor(Trips4_1$household_code)
Trips4_1$week <- factor(Trips4_1$week)
Trips4_1$month <- factor(Trips4_1$month)
Trips4_1$weekR <- factor(Trips4_1$weekR)
Trips4_1$monthR <- factor(Trips4_1$monthR)


#########################################################################



##############################
source("EIS/Code/KaplanNew/gmmq.R")
#source("Code/Quantile/gmmq.R")
##############################

#########################################################################

print("a")
#YFE <- plm(Y ~ month, data=Trips4_1, model='within', index=c('household_code', 'weekR'))
#YTilde <- YFE$resid

print("b")
#XFE <- plm(LogR ~ month, data=Trips4_1, model='within', index=c('household_code', 'weekR'))
#XTilde <- XFE$resid

print("c")
#Z1FE <- plm(YInst ~ month, data=Trips4_1, model='within', index=c('household_code', 'weekR'))
#Z1Tilde <- Z1FE$resid

print("d")
#Z2FE <- plm(Lag2LogNomR ~ month, data=Trips4_1, model='within', index=c('household_code', 'weekR'))
#Z2Tilde <- Z2FE$resid

print("e")
#Z3FE <- plm(Lag2Inf ~ month, data=Trips4_1, model='within', index=c('household_code', 'weekR'))
#Z3Tilde <- Z3FE$resid

Y <- Trips4_1$Y
LogR <- Trips4_1$LogR
Xvar <- Trips4_1$household_income

print("f")
#Z.excl <- cbind(Z1Tilde, Z2Tilde, Z3Tilde)
Z.inst1<-lm(Y~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.inst2<-lm(LogR~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.inst3<-lm(Xvar~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.excl <- cbind(Z.inst1,Z.inst2,Z.inst3)
Y <- cbind(Y,LogR)
X <- cbind(matrix(data=1,ncol=1,nrow=nrow(Y)), Xvar)


print("g")
#PLM <- plm(Y ~ LogR + Xvar + month | YInst + Lag2LogNomR + Lag2Inf + Xvar + month, data=Trips4_1, model='pooling', index=c('household_code', 'weekR'))
PLM <- plm(Y ~ LogR + Xvar | YInst + Lag2LogNomR + Lag2Inf + Xvar, data=Trips4_1, model='pooling', index=c('household_code', 'weekR'))
summary(PLM)
PLM$coef[1]

print("h")


# Yogo (2004) 2SLS log-linear estimator
#aa <- solve(t(cbind(1,Z.excl))%*%cbind(1,Z.excl))
#print("1.1")
#bb <- cbind(1,Z.excl) %*% aa
#print("1.2")
#PZ <- bb %*% t(cbind(1,Z.excl))


#PZ <- cbind(1,Z.excl) %*% solve(t(cbind(1,Z.excl))%*%cbind(1,Z.excl)) %*% t(cbind(1,Z.excl))


#b.2sls <- solve(t(cbind(X,Y[,2]))%*%PZ%*%cbind(X,Y[,2])) %*% (t(cbind(X,Y[,2]))%*%PZ%*%Y[,1])

#print("3")

## SETUP for quantile GMM
conv.fn <- function(b) c(exp(b[1]/b[2]), 1/b[2]) #convert log-linear parameters to (beta,gamma)
conv.inv.fn <- function(b) c(log(b[1])/b[2], 1/b[2])
conv2.fn <- function(b) c(exp(b[1]/b[2]), b[2]) #convert log-linear parameters to (beta,EIS)
conv2.inv.fn <- function(b) c(log(b[1])*b[2], b[2])
conv3.fn <- function(b) c(b[1],1/b[2]) # convert (beta,gamma) to (beta,EIS)
conv3.inv.fn <- function(b) c(b[1],1/b[2]) 
# 
# Residual functions (and derivatives thereof) for quantile GMM
Lfn <- function(y,x,b) y[,1]-cbind(x,y[,-1])%*%b  #log-linear
Ldfn <- function(y,x,b) -cbind(x,y[,-1])
Lfn2b <- function(y,x,b) -Lfn(y,x,b) #-y[,1]+cbind(x,y[,-1])%*%b #log-linear, 1-tau
Ldfn2b <- function(y,x,b) -Ldfn(y,x,b) #cbind(x,y[,-1])
Lfn2 <- function(y,x,b) b[1]*exp(y[,1])^(-b[2])*exp(y[,2]) - 1 #nonlinear (beta,gamma)
Ldfn2 <- function(y,x,b) cbind((Lfn2(y=y,x=x,b=b)+1) / b[1], 
                               -y[,1]*(Lfn2(y=y,x=x,b=b)+1))
Lfn22 <- function(y,x,b) b[1]*exp(y[,1])^(-1/b[2])*exp(y[,2]) - 1 #nonlinear (beta,gamma)
Ldfn22 <- function(y,x,b) cbind((Lfn2(y=y,x=x,b=b)+1) / b[1], 
                               y[,1]*(Lfn2(y=y,x=x,b=b)+1)/b[2]^2)

# Sanity check: large h => replicates 2SLS
#taus<-seq(0.2,0.8,0.05)

dimX <- 3
H.HUGE <- 0.001 
#H.HUGE <- 0.0001
n<-nrow(Trips4_1)
tau<-seq(0.1,0.9,0.1)
nt<-length(tau)
#nt<-29
#tau<-(1:nt)/(nt+1)
coef.beta<-array(0,dim=c(nt,1))
coef.eis<-array(0,dim=c(nt,1))

head(Y)
head(X)
head(Z.excl)

for (i in 1:nt){

    print(tau[i])

    ret2b <- tryCatch(gmmq(tau=tau[i], dB=dimX, Y=Y, X=X, Z.excl=Z.excl,
              Lambda=Lfn2b, Lambda.derivative=Ldfn2b,
              h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE,          b.init=c(.99,PLM$coef[1])),
             error=function(w)list(b=c(NA,NA),h=NA))


    coef.beta[i]<-conv2.fn(ret2b$b)[1]
    coef.eis[i]<-conv2.fn(ret2b$b)[2]
    
    print(coef.beta[i])
    print(coef.eis[i])

}


QGMMResults <- cbind(tau,coef.beta,coef.eis)
colnames(QGMMResults) <- c("tau", "Beta", "EIS")

print(QGMMResults)

write.csv(QGMMResults, "EIS/Output/QGMM_PooledResultsAll_covariates_noSE.csv", row.names=FALSE)

