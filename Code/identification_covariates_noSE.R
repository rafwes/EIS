
library(tidyverse)

setwd('/Users/lancecundy/Documents/Research/Nielsen/EIS')

mydata <- read.csv('UseData/SampleData.csv')

# mydata %>%
#   group_by(household_code) %>%
#   count() %>%
#   ungroup()
# 
# # For each household i, construct a gride of quantiles, /tau
# 
# mydata.i <- mydata %>%
#   filter(household_code == 2000000)

mydata.i <- mydata %>%
  group_by(household_code) %>% 
  mutate(household_income = rand(1)*100) %>%
  ungroup()

source("Code/dCGKL_2018_code/gmmq.R")


## Setup Data
Y <- as.matrix(mydata.i$Y)
n <- nrow(Y)

#X.excl <- matrix(data=1, nrow=n, ncol=1)
X.excl <- cbind(matrix(data=1, nrow=n, ncol=1), mydata.i$household_income)
D <- as.matrix(mydata.i$LogR)

Z.excl <- as.matrix(cbind(mydata.i$YInst, mydata.i$Lag2LogNomR, mydata.i$Lag2Inf))
Z <- cbind(Z.excl, X.excl)
X <- cbind(D, X.excl)

## Yogo (2004) 2SLS log-linear estimator as a starting point
PZ <- Z %*% solve(t(Z)%*%Z) %*% t(Z)
StartingPointReg <- solve(t(X)%*%PZ%*%X) %*% (t(X)%*%PZ%*%Y) 

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
dimX <- 3
H.HUGE <- 0.001

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
  coef.beta[i]<-conv2.fn(ret2b$b)[1]
  coef.eis[i]<-conv2.fn(ret2b$b)[2]

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

#QGMMResults <- cbind(tau,coef.beta,se.beta,coef.eis,se.eis,band.eis)
#colnames(QGMMResults) <- c("tau", "Beta", "Beta.SE", "EIS", "EIS.SE", "EIS.Band")

QGMMResults <- cbind(tau,coef.beta,coef.eis)
colnames(QGMMResults) <- c("tau", "Beta", "EIS")

print(QGMMResults)

#write.csv(QGMMResults, "EIS/Output/QGMM_PooledResultsP1_Adjusted3.csv", row.names=FALSE)

