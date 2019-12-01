

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

# MyJob.po8036441 P3
#MyJob.po8039349 P1

########################################################################
########################################################################

Trips4 <- read.csv('Datasets/RealTripsChangesWeeklyIRAllTime4.csv')
Trips4 <- Trips4[c('household_code', 'week', 'month', 'weekR', 'monthR', 'year', 'Y', 'LogR', 'YInst', 'Lag2LogNomR', 'Lag2Inf')]

#print(unique(cbind(Trips4$year, Trips4$month)))

Trips4_1a <- Trips4[which(Trips4$year == 2004 & Trips4$month == 2),]
Beg1 <- unique(Trips4_1a$monthR)
Trips4_1b <- Trips4[which(Trips4$year == 2006 & Trips4$month == 8),]
End1 <- unique(Trips4_1b$monthR)

#Trips4_2a <- Trips4[which(Trips4$year == 2006 & Trips4$month == 9),]
#Beg2 <- unique(Trips4_2a$monthR)
#Trips4_2b <- Trips4[which(Trips4$year == 2008 & Trips4$month == 12),]
#End2 <- unique(Trips4_2b$monthR)

#Trips4_3a <- Trips4[which(Trips4$year == 2009 & Trips4$month == 1),]
#Beg3 <- unique(Trips4_3a$monthR)
#Trips4_3b <- Trips4[which(Trips4$year == 2014 & Trips4$month == 12),]
#End3 <- unique(Trips4_3b$monthR)

print(Beg1)
print(End1)
#print(Beg2)
#print(End2)
#print(Beg3)
#print(End3)


########################################################################
########################################################################


Trips4_1 <- Trips4[which(Trips4$monthR >= Beg1 & Trips4$monthR <= End1),]
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

print("f")
#Z.excl <- cbind(Z1Tilde, Z2Tilde, Z3Tilde)
Z.inst1<-lm(Y~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.inst2<-lm(LogR~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
Z.excl <- cbind(Z.inst1,Z.inst2)
Y <- cbind(Y,LogR)
X <- matrix(data=1,ncol=1,nrow=nrow(Y))


print("g")
PLM <- plm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4_1, model='pooling', index=c('household_code', 'weekR'))
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

#H.HUGE <- 0.001 
H.HUGE <- 0.0001
n<-nrow(Trips4_1)
tau<-seq(0.1,0.9,0.1)
nt<-length(tau)
#nt<-29
#tau<-(1:nt)/(nt+1)
coef.beta<-array(0,dim=c(nt,1))
coef.eis<-array(0,dim=c(nt,1))
se.beta<-array(0,dim=c(nt,1))
se.eis<-array(0,dim=c(nt,1))
band.eis<-array(0,dim=c(nt,1))

#band<-seq(0.05,0.95,0.45)
band <- 1.4
nb<-length(band)

for (i in 1:nt){
    #tau <- tau[i]
    #tau = 0.5 
    #print(tau)
    #H.HUGE <- .001
    print(tau[i])
    #print("A")
    #ret <- tryCatch(gmmq(tau=tau[i],dB=2,Y=Y,X=X,Z.excl=Z.excl,
    #            Lambda=Lfn, Lambda.derivative=Ldfn,
    #            h=H.HUGE, VERBOSE=FALSE, RETURN.Z=TRUE, b.init=c(.99,PLM$coef[1])),
    #             error=function(w)list(b=c(NA,NA),h=NA))
    #             print("B")
    ret2b <- tryCatch(gmmq(tau=tau[i], dB=2, Y=Y, X=X, Z.excl=Z.excl,
              Lambda=Lfn2b, Lambda.derivative=Ldfn2b,
              h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE,          b.init=c(.99,PLM$coef[1])),
             error=function(w)list(b=c(NA,NA),h=NA))
             #print("C")
#ret2 <- tryCatch(gmmq(tau=tau[i], dB=2, Y=Y, X=X, Z.excl=Z.excl,
#             Lambda=Lfn2, Lambda.derivative=Ldfn2,
#             h=H.HUGE, VERBOSE=TRUE, RETURN.Z=TRUE, b.init=conv.fn(c(.99,PLM$coef[1]))),
#             error=function(w)list(b=c(NA,NA),h=NA))
#ret22 <- tryCatch(gmmq(tau=tau[i], dB=2, Y=Y, X=X, Z.excl=Z.excl,
#             Lambda=Lfn22, Lambda.derivative=Ldfn22,
#             h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE, b.init=conv2.fn(c(.99,PLM$coef[1]))),
#             error=function(w)list(b=c(NA,NA),h=NA))


    #ret22 <- c(NA,NA) 

    # 2SLS, Log-Linear, and NonLinear estimates
    #print(tau)
    #Results <- cbind(c(NA,PLM$coef[1]), conv2.fn(ret$b), conv2.fn(ret2b$b), conv3.fn(ret2$b) )#, ret22$b)
    #colnames(Results) <- c("PLM", "Log-Linear", "Log-Linear", "NonLinear") #, "NonLinear")
    #rownames(Results) <- c("Beta", "EIS")
    #print(Results)

    # The fourth column above represents the non-linear estimates following Yogo (2004). Here it is again.
    #cat("\nNonLinear Median Estimates Following Yogo (2004):", conv3.fn(ret2$b), "\n") 


    coef.beta[i]<-conv2.fn(ret2b$b)[1]
    coef.eis[i]<-conv2.fn(ret2b$b)[2]

    g.theta1<-1/(coef.beta[i]*coef.eis[i])
    g.theta2<--log(coef.beta[i])*(1/coef.eis[i]^2)
    g.theta<-c(g.theta1,g.theta2)

    se.beta.t<-array(0,dim=c(nb,1))
    se.eis.t<-array(0,dim=c(nb,1))

    for (j in 1:nb){

        #band<-bandwidth.rq(tau[i],n)

        print(band[j])

        cov.est <- cov.est.fn(tau=tau[i],Y=Y,X=X,Z=Z.excl,Lambda=Lfn2b,Lambda.derivative=Ldfn2b,beta.hat=ret2b$b,Itilde=Itilde.KS17,Itilde.deriv=Itilde.deriv.KS17,h=H.HUGE,structure=c('ts'),cluster.X.col=0,LRV.kernel=c('Bartlett'),LRV.ST=NA,VERBOSE=FALSE,h.adj=band[j])

        # Estimate asymptotic covariance matrix
        # h.adj: should be strictly between 0 and 1/2 if "h=0" (smallest possible bandwidth) was used for estimation, to adjust h to better estimate G; if h.adj=1, then same h is used for G and Sigma estimation (i.e., the value passed in argument h).
        #tau=tau[i]
        #Z=Z.excl
        #Lambda=Lfn2b
        #Lambda.derivative=Ldfn2b
        #beta.hat=ret2b$b
        #Itilde=Itilde.KS17
        #Itilde.deriv=Itilde.deriv.KS17
        #h=H.HUGE
        #structure=c('ts')
        #cluster.X.col=0
        #LRV.kernel=c('Bartlett')
        #LRV.ST=NA
        #VERBOSE=FALSE
        #h.adj=band[j]


        # if (missing(structure) || !is.character(structure)) stop("Argument structure must be 'iid' or 'ts' or 'cluster'")
        #structure <- match.arg(structure)
        #LRV.kernel <- match.arg(LRV.kernel)
  
  
  
  
        #G.hat <- G.est.fn(Y=Y,X=X,Z=Z,Lambda=Lambda,Lambda.derivative=Lambda.derivative,beta.hat=beta.hat,Itilde.deriv=Itilde.deriv,h=h^h.adj,VERBOSE=VERBOSE)
  
        #h=h^h.adj

        #nG <- dim(Z)[1]
        #L <- Lambda(Y,X,beta.hat)
        #Ld <- Lambda.derivative(Y,X,beta.hat)
        # tmpsum <- array(0,dim=c(dim(Z)[2],length(beta.hat)))
        # for (i in 1:n) {
            #   tmp <- Itilde.deriv(-L[i]/h) *
            #     matrix(Z[i,],ncol=1) %*% matrix(Ld[i,], nrow=1)
            #   tmpsum <- tmpsum + tmp
            # }
        #tmpsum2 <- t(array(data=Itilde.deriv(-L/h),dim=dim(Z)) * Z) %*% Ld
        #G.hat <- (-tmpsum2/(nG*h))
        #print("Ghat")
        #print(G.hat)
        #Ginv <- tryCatch(solve(G.hat), error=function(w)NA)
        #Ginv <- tryCatch(ginv(G.hat), error=function(w)NA)
        #print("Ginv")
        #print(Ginv)
        #if (is.na(Ginv[1])) cov.est <- NA else {
        #    print("LRV")
        #    LRV.hat <- LRV.est.fn(tau=tau,Y=Y,X=X,Z=Z,Lambda=Lambda,beta.hat=beta.hat,Itilde=Itilde,h=h,structure=structure,cluster.X.col=cluster.X.col,LRV.kernel=LRV.kernel,LRV.ST=LRV.ST,VERBOSE=VERBOSE)
         #   print(LRV.hat)
    
          #  cov.est <- (Ginv %*% LRV.hat %*% t(Ginv))
        #}

        print("cov")
        print(cov.est)

        if (is.na(cov.est)) {
            se.beta.t[j] <- NA
            se.eis.t[j] <- NA
        } else {
            cov <- cov.est

            cov_beta<-g.theta%*%cov%*%g.theta
            cov_eis<-cov[2,2]

            se.beta.t[j]<-sqrt(cov_beta/n)
            se.eis.t[j]<-sqrt(cov_eis/n)
        }

        print("se.eis.t")
        print(se.eis.t[j])


    }
    print("Made it through")
    print(se.eis.t)
    print(which.min(se.eis.t))
    print(band[which.min(se.eis.t)])
    print(se.beta.t[which.min(se.eis.t)])
    print(se.eis.t[which.min(se.eis.t)])
    #cbind(se.beta.t,se.eis.t)
    #which.min(se.eis.t)
    MinLoc <- which.min(se.eis.t)
    finalband <- band[MinLoc]
    min.se.beta <- se.beta.t[MinLoc]
    min.se.eis <- se.eis.t[MinLoc]

    #cbind(coef.beta,se.beta,coef.eis,se.eis)
    #cbind(bf.2sls[1],se.beta.iv,bf.2sls[2],se.eis.iv)

    #if (is.na(cov.est)) {
    #    cov <- matrix(NA,2,2)
    #} else {
    #    cov <- cov.est
    #}

    #print(cov)


    #cov_beta<-cov[1,1]
    #cov_eis<-(conv2.fn(ret2b$b)[2]^4)*cov[2,2]

    se.beta[i]<-min.se.beta
    se.eis[i]<-min.se.eis
    band.eis[i] <- finalband

}


QGMMResults <- cbind(tau,coef.beta,se.beta,coef.eis,se.eis,band.eis)
colnames(QGMMResults) <- c("tau", "Beta", "Beta.SE", "EIS", "EIS.SE", "EIS.Band")

print(QGMMResults)

write.csv(QGMMResults, "EIS/Output/QGMM_PooledResultsP1_Adjusted3.csv", row.names=FALSE)


#print("5")

#Z.excl <- cbind(TripsM[,'Change2LogC'], TripsM[,'Lag2LogNomR'], TripsM[,'Lag2Inf'] )
#Y <- cbind(TripsM[,'ChangeLogC'],TripsM[,'LogR'])
#print("5.25")
#X <- sparse.model.matrix(~TripsM$household_code + TripsM$month)

#class(Y)
#print("5.25")
#class(X)

#print("5.5")

#head(X,3)

#(XtX)^-1 Xty

#solve(t(cbind(Y[,2],X))%*%cbind(Y[,2],X)) %*% t(cbind(Y[,2],X)) %*% Y[,1]

#PLM <- plm(ChangeLogC ~ LogR + month, data=TripsM, model='within', index=c('household_code', 'monthR'))
#summary(PLM)

#print("6")

#YogoRegM <- plm(ChangeLogC ~ LogR + month | Change2LogC + Lag2LogNomR + Lag2Inf + month, data=TripsM, model='within', index=c('household_code', 'monthR'))
#summary(YogoRegM)

# Yogo (2004) 2SLS log-linear estimator
#PZ <- cbind(1,Z.excl) %*% solve(t(cbind(1,Z.excl))%*%cbind(1,Z.excl)) %*% t(cbind(1,Z.excl))

#solve(t(cbind(X,Y[,2]))%*%PZ%*%cbind(X,Y[,2]))

#b.2sls <- solve(t(cbind(X,Y[,2]))%*%PZ%*%cbind(X,Y[,2])) %*% (t(cbind(X,Y[,2]))%*%PZ%*%Y[,1])

#b.2sls

#print("7")

# Residual functions (and derivatives thereof) for quantile GMM
#Lfn <- function(y,x,b) y[,1]-cbind(x,y[,-1])%*%b  #log-linear
#Ldfn <- function(y,x,b) -cbind(x,y[,-1])

#-cbind(X,Y[,-1])

#print("7.5")

# Sanity check: large h => replicates 2SLS
#tau <- 0.5;  H.HUGE <- 100
#ret <- gmmq(tau=tau,dB=2,Y=Y,X=X,Z.excl=Z.excl,
#            Lambda=Lfn, Lambda.derivative=Ldfn,
#            h=H.HUGE, VERBOSE=FALSE, RETURN.Z=TRUE, b.init=c(0.98,0.2))

#print("8")