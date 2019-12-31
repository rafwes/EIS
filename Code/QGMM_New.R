
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

source("EIS/Code/dCGKL_2018_code/gmmq.R")

#########################################################################


## Setup Data
Y <- as.matrix(Trips4_1$Y)
n <- nrow(Y)

X.excl <- matrix(data=1, nrow=n, ncol=1)
D <- as.matrix(Trips4_1$LogR)


Z.inst1<-lm(Y~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.inst2<-lm(LogR~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
#Z.inst3<-lm(Xvar~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.excl <- cbind(Z.inst1,Z.inst2)
Z <- cbind(Z.excl, X.excl)
X <- cbind(D, X.excl)

## Yogo (2004) 2SLS log-linear estimator as a starting point
PZ <- Z %*% solve(t(Z)%*%Z) %*% t(Z)
StartingPointReg <- solve(t(X)%*%PZ%*%X) %*% (t(X)%*%PZ%*%Y) 

#PLM <- plm(Y ~ LogR + Xvar + month | YInst + Lag2LogNomR + Lag2Inf + Xvar + month, data=Trips4_1, model='pooling', index=c('household_code', 'weekR'))
PLM <- plm(Y ~ LogR + Xvar | YInst + Lag2LogNomR + Lag2Inf + Xvar, data=Trips4_1, model='pooling', index=c('household_code', 'weekR'))
summary(PLM)
StartingPointReg <- c(PLM$coef[2], PLM$coef[1])

####################################################################
##### Setup Functions #####
####################################################################

# conv.fn convert log-linear fn's (b[1],b[2])=(slope, constant) to (beta, gamma)
conv.fn <- function(b) c(exp(b[2]/b[1]), 1/b[1]) #convert log-linear parameters to (beta,gamma)
conv.inv.fn <- function(b) c(log(b[2])/b[1], 1/b[1])

# conv.fn convert log-linear fn's (b[1],b[2])=(slope, constant) to (beta, EIS)
conv2.fn <- function(b) c(exp(b[2]/b[1]), b[1]) #convert log-linear parameters to (beta,EIS)
conv2.inv.fn <- function(b) c(log(b[2])*b[1], b[1])

conv3.fn <- function(b) c(b[1],1/b[2]) # convert (beta,gamma) to (beta,EIS)
conv3.inv.fn <- function(b) c(b[1],1/b[2]) 

# Residual/Lambda functions (and derivatives) for smoothed MM estimation
Lfn.gmmq <- function(y,x,b) y[,1]-cbind(y[,-1],x)%*%b  #log-linear
Ldfn.gmmq <- function(y,x,b) -cbind(y[,-1],x)
Lfn2b.gmmq <- function(y,x,b) -Lfn.gmmq(y,x,b) #-y[,1]+cbind(x,y[,-1])%*%b #log-linear, 1-tau
Ldfn2b.gmmq <- function(y,x,b) -Ldfn.gmmq(y,x,b) #cbind(x,y[,-1])

Lfn2.gmmq <- function(y,x,b) b[1]*exp(y[,1])^(-b[2])*exp(y[,2]) - 1 #nonlinear (beta,gamma)
Ldfn2.gmmq <- function(y,x,b) cbind((Lfn2.gmmq(y=y,x=x,b=b)+1) / b[1], 
                                    -y[,1]*(Lfn2.gmmq(y=y,x=x,b=b)+1))

Lfn22.gmmq <- function(y,x,b) b[1]*exp(y[,1])^(-1/b[2])*exp(y[,2]) - 1 #nonlinear (beta,EIS)
Ldfn22.gmmq <- function(y,x,b) cbind((Lfn2.gmmq(y=y,x=x,b=b)+1) / b[1], 
                                     y[,1]*(Lfn2.gmmq(y=y,x=x,b=b)+1)/b[2]^2)


# Residual/Lambda functions (and derivatives) for smoothed GMM estimation
Lfn <- function(y,x,b) y-x%*%b  #log-linear
Ldfn <- function(y,x,b) -x
Lfn2b <- function(y,x,b) -Lfn(y,x,b) #-y+x%*%b #log-linear, 1-tau
Ldfn2b <- function(y,x,b) -Ldfn(y,x,b) #x

Lfn2 <- function(y,x,b) b[1]*exp(y)^(-b[2])*exp(x[,1]) - 1 #nonlinear (beta,gamma)
Ldfn2 <- function(y,x,b) cbind((Lfn2(y=y,x=x,b=b)+1) / b[1], 
                               -y*(Lfn2(y=y,x=x,b=b)+1))

Lfn22 <- function(y,x,b) b[1]*exp(y)^(-1/b[2])*exp(x[,1]) - 1 #nonlinear (beta,gamma)
Ldfn22 <- function(y,x,b) cbind((Lfn2(y=y,x=x,b=b)+1) / b[1], 
                                y*(Lfn2(y=y,x=x,b=b)+1)/b[2]^2)
####################################################################


## Initialize  Variables
dimX <- ncol(X)
H.HUGE <- 0.0001

tau<-seq(0.1,0.9,0.1)
nt<-length(tau)

coef.beta<-array(0,dim=c(nt,1))
coef.eis<-array(0,dim=c(nt,1))
se.beta<-array(0,dim=c(nt,1))
se.eis<-array(0,dim=c(nt,1))
band.eis<-array(0,dim=c(nt,1))

#band <- 1
band<-seq(0.05,0.95,0.45)
nb<-length(band)

for (i in 1:nt){
  
  print(tau[i])
  
  # GMMQ Function
  ret2b <- tryCatch(gmmq(tau=tau[i], dB=dimX, Y=cbind(Y,D), X=X.excl, Z.excl=Z.excl,
                            Lambda=Lfn2b.gmmq, Lambda.derivative=Ldfn2b.gmmq,
                            h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE, b.init=StartingPointReg),
                       error=function(w)list(b=c(NA,NA),h=NA))
  
  # Get Coefficients 
  coef.beta[i]<-conv2.fn(ret2b$b)[2]
  coef.eis[i]<-conv2.fn(ret2b$b)[1]
  
  # # Get G 
  # g.theta1<-1/(coef.beta[i]*coef.eis[i])
  # g.theta2<--log(coef.beta[i])*(1/coef.eis[i]^2)
  # g.theta<-c(g.theta1,g.theta2)
  # 
  # # Create empty SE matrix
  # se.beta.t<-array(0,dim=c(nb,1))
  # se.eis.t<-array(0,dim=c(nb,1))
  # 
  # for (j in 1:nb){
  #   
  #   print(band[j])
  #   
  #   # Get Covariance
  #   cov.est <- cov.est.fn(tau=tau,Y=cbind(Y,D),X=X.excl,Z=Z.excl,Lambda=Lfn2b.gmmq,Lambda.derivative=Ldfn2b.gmmq,beta.hat=ret2b$b,Itilde=Itilde.KS17,Itilde.deriv=Itilde.deriv.KS17,h=H.HUGE,structure=c('ts'),cluster.X.col=0,LRV.kernel=c('Bartlett'),LRV.ST=NA,VERBOSE=FALSE,h.adj=band[j])
  #   
  #   
  #   # Y=cbind(Y,D)
  #   # X=X.excl
  #   # Z=Z.excl
  #   # Lambda=Lfn2b.gmmq
  #   # Lambda.derivative=Ldfn2b.gmmq
  #   # beta.hat=ret2b$b
  #   # Itilde.deriv=Itilde.deriv.KS17
  #   # h=H.HUGE
  #   # VERBOSE=FALSE
  #   #   n <- dim(Z)[1]
  #   #   L <- Lfn2b.gmmq(Y,X,beta.hat)
  #   #   Ld <- Ldfn2b.gmmq(Y,X,beta.hat)
  #   #   # tmpsum <- array(0,dim=c(dim(Z)[2],length(beta.hat)))
  #   #   # for (i in 1:n) {
  #   #   #   tmp <- Itilde.deriv(-L[i]/h) *
  #   #   #     matrix(Z[i,],ncol=1) %*% matrix(Ld[i,], nrow=1)
  #   #   #   tmpsum <- tmpsum + tmp
  #   #   # }
  #   #   tmpsum2 <- t(array(data=Itilde.deriv(-L/h),dim=dim(Z)) * Z) %*% Ld
  #   #   G.hat <- (-tmpsum2/(n*h))
  #   #   
  #   #   Ginv <- tryCatch(solve(G.hat))
  #   
  #   print("cov")
  #   print(cov.est)
  #   
  #   # Get SE when the cov matrix comes out
  #   if (all(is.na(cov.est))) {
  #     se.beta.t[j] <- NA
  #     se.eis.t[j] <- NA
  #   } else {
  #     cov <- cov.est
  #     
  #     cov_beta<-g.theta%*%cov%*%g.theta
  #     cov_eis<-cov[2,2]
  #     
  #     se.beta.t[j]<-sqrt(cov_beta/n)
  #     se.eis.t[j]<-sqrt(cov_eis/n)
  #   }
  #   
  #   print("se.eis.t")
  #   print(se.eis.t[j])
  #   
  #   
  # }
  # print("Made it through")
  # print(se.eis.t)
  # print(which.min(se.eis.t))
  # print(band[which.min(se.eis.t)])
  # print(se.beta.t[which.min(se.eis.t)])
  # print(se.eis.t[which.min(se.eis.t)])
  # 
  # # Get minimum SE
  # MinLoc <- which.min(se.eis.t)
  # finalband <- band[MinLoc]
  # min.se.beta <- se.beta.t[MinLoc]
  # min.se.eis <- se.eis.t[MinLoc]
  # 
  # # Keep minimum SE
  # se.beta[i]<-min.se.beta
  # se.eis[i]<-min.se.eis
  # band.eis[i] <- finalband
  
}

QGMMResults <- cbind(tau,coef.beta,se.beta,coef.eis,se.eis,band.eis)
colnames(QGMMResults) <- c("tau", "Beta", "Beta.SE", "EIS", "EIS.SE", "EIS.Band")

print(QGMMResults)

write.csv(QGMMResults, "EIS/Output/QGMM_New_PooledResultsAll.csv", row.names=FALSE)

