
### Set these paths ###
setwd('/Users/lancecundy/Documents/Research/Nielsen/EIS/Code')
source("dCGKL_2018_code/gmmq.R")
dat <- read.table(file=paste0('dCGKL_2018_code/Yogo/USAQ.txt'),header=TRUE,sep="",na.strings=c("NA",".")) 
#######################

dat <- dat[-(1:2),] #remove missing instruments first two quarters
Z.excl <- as.matrix(dat[,9:12])
Y <- as.matrix(dat[,'dc'])
n <- nrow(Y)

X.excl <- matrix(data=1,nrow=n, ncol=1)
D <- as.matrix(dat[,'rrf'])

Z.excl <- as.matrix(dat[,9:12])
Z <- cbind(Z.excl, X.excl)
X <- cbind(D, X.excl)

# Yogo (2004) 2SLS log-linear estimator
PZ <- Z %*% solve(t(Z)%*%Z) %*% t(Z)
b.2sls <- solve(t(X)%*%PZ%*%X) %*% (t(X)%*%PZ%*%Y) 


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
H.HUGE <- 0.0001
tau<-0.5
band <- 0.05

# GMMQ Function
ret2b <- tryCatch(gmmq(tau=tau, dB=2, Y=cbind(Y,D), X=X.excl, Z.excl=Z.excl,
                       Lambda=Lfn2b.gmmq, Lambda.derivative=Ldfn2b.gmmq,
                       h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE, b.init=b.2sls),
                  error=function(w)list(b=c(NA,NA),h=NA))
# Get Covariance
cov.est <- cov.est.fn(tau=tau,Y=cbind(Y,D),X=X.excl,Z=Z.excl,Lambda=Lfn2b.gmmq,Lambda.derivative=Ldfn2b.gmmq,beta.hat=ret2b$b,Itilde=Itilde.KS17,Itilde.deriv=Itilde.deriv.KS17,h=H.HUGE,structure=c('ts'),cluster.X.col=0,LRV.kernel=c('Bartlett'),LRV.ST=NA,VERBOSE=FALSE,h.adj=band)

cov.est # It comes out empty, because G.hat is not square, so we cannot take the inverse

G.hat <- G.est.fn(Y=cbind(Y,D),X=X.excl,Z=Z.excl,Lambda=Lfn2b.gmmq,Lambda.derivative=Ldfn2b.gmmq,beta.hat=ret2b$b,Itilde.deriv=Itilde.deriv.KS17,h=H.HUGE,VERBOSE=FALSE)

G.hat 

