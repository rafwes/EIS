

QGMM.fn <- funtion(Trips4_1, tau=seq(0.1,0.9,0.1), H.HUGE=0.0001, band=c(0.05,0.95,1.45)) {
  
  # Separate Y and X from Data
  Y <- Trips4_1$Y
  LogR <- Trips4_1$LogR
  
  # Set up instruments
  Z.inst1<-lm(Y~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
  Z.inst2<-lm(LogR~YInst+Lag2LogNomR+Lag2Inf, data=Trips4_1)$fitted
  Z.excl <- cbind(Z.inst1,Z.inst2)
  
  # Create Y and X
  Y <- cbind(Y,LogR)
  X <- matrix(data=1,ncol=1,nrow=nrow(Y))
  
  # Run FE regression to get starting points
  StartingPointReg <- lm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4_1)
  #StartingPointReg <- plm(Y ~ LogR + month | YInst + Lag2LogNomR + Lag2Inf + month, data=Trips4_1, model='pooling', index=c('household_code', 'weekR'))
  summary(StartingPointReg)
  StartingPointReg$coef[1]
  
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
  
  # Setup
  n<-nrow(Trips4_1)
  nt<-length(tau)
  nb<-length(band)

  coef.beta<-array(0,dim=c(nt,1))
  coef.eis<-array(0,dim=c(nt,1))
  se.beta<-array(0,dim=c(nt,1))
  se.eis<-array(0,dim=c(nt,1))
  band.eis<-array(0,dim=c(nt,1))
  
  for (i in 1:nt){
    
    print(tau[i])

    # GMMQ Function
    ret2b <- tryCatch(gmmq(tau=tau[i], dB=2, Y=Y, X=X, Z.excl=Z.excl,
                           Lambda=Lfn2b, Lambda.derivative=Ldfn2b,
                           h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE,          b.init=c(.99,StartingPointReg$coef[1])),
                      error=function(w)list(b=c(NA,NA),h=NA))

    # Get Coefficients 
    coef.beta[i]<-conv2.fn(ret2b$b)[1]
    coef.eis[i]<-conv2.fn(ret2b$b)[2]
    
    # Get G 
    g.theta1<-1/(coef.beta[i]*coef.eis[i])
    g.theta2<--log(coef.beta[i])*(1/coef.eis[i]^2)
    g.theta<-c(g.theta1,g.theta2)
   
    # Create empty SE matrix
    se.beta.t<-array(0,dim=c(nb,1))
    se.eis.t<-array(0,dim=c(nb,1))
    
    for (j in 1:nb){
      
      print(band[j])
      
      # Get Covariance
      cov.est <- cov.est.fn(tau=tau[i],Y=Y,X=X,Z=Z.excl,Lambda=Lfn2b,Lambda.derivative=Ldfn2b,beta.hat=ret2b$b,Itilde=Itilde.KS17,Itilde.deriv=Itilde.deriv.KS17,h=H.HUGE,structure=c('ts'),cluster.X.col=0,LRV.kernel=c('Bartlett'),LRV.ST=NA,VERBOSE=FALSE,h.adj=band[j])
      
      print("cov")
      print(cov.est)
      
      # Get SE when the cov matrix comes out
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
    
    # Get minimum SE
    MinLoc <- which.min(se.eis.t)
    finalband <- band[MinLoc]
    min.se.beta <- se.beta.t[MinLoc]
    min.se.eis <- se.eis.t[MinLoc]
    
    # Keep minimum SE
    se.beta[i]<-min.se.beta
    se.eis[i]<-min.se.eis
    band.eis[i] <- finalband
    
  }
  
  
  QGMMResults <- cbind(tau,coef.beta,se.beta,coef.eis,se.eis,band.eis)
  colnames(QGMMResults) <- c("tau", "Beta", "Beta.SE", "EIS", "EIS.SE", "EIS.Band")
  
  print(QGMMResults)
  
  #write.csv(QGMMResults, "EIS/Output/QGMM_PooledResultsP1_Adjusted3.csv", row.names=FALSE)

  
}

