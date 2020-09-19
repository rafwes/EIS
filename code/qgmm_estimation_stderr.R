rm(list = ls())
library(plm)
library(tidyverse)

channelTypes <- c("every_channel",
                  "grocery_channel")

# Set cluster path
base.path <- "/xdisk/agalvao/mig2020/extra/agalvao/eis_nielsen/rafael"
# Set local sample path
base.path <- "/home/rafael/Sync/IMPA/2020.0/simulations/code"

# Run each separately due to long calculation times!
# panelistsSelectionList <- 
#   c("all",
#     "income_less25k",
#     "income_geq25k",
#     "income_geq50k",
#     "income_geq70k",
#     "age_malehead_leq34",
#     "age_malehead_geq35leq54",
#     "age_malehead_geq55",
#     "age_femalehead_leq34",
#     "age_femalehead_geq35leq54",
#     "age_femalehead_geq55",
#     "college_both",
#     "college_one",
#     "college_none",
#     "ethnicity_white",
#     "ethnicity_black",
#     "ethnicity_asian",
#     "ethnicity_hispanic"
#   )

#panelistSelection <- panelistSelectionList[1]

# Run each separately due to long calculation times!
panelistSelection  <- 
    # "all"
    # "income_less25k"
    # "income_geq25k"
    # "income_geq50k"
    # "income_geq70k"
    # "age_malehead_leq34"
    # "age_malehead_geq35leq54"
    "age_malehead_geq55"
    # "age_femalehead_leq34"
    # "age_femalehead_geq35leq54"
    # "age_femalehead_geq55"
    # "college_both"
    # "college_one"
    # "college_none"
    # "ethnicity_white"
    # "ethnicity_black"
    # "ethnicity_asian"
    # "ethnicity_hispanic"

# Label Period
period <- c("ALL")
# The code was originally set up to split the data as desired
# The dates are currently set to encompass everything
dateSplitStarts <- c("2004-01-01")
dateSplitEnds <- c("2018-01-01")
dateSplits <- cbind(period, 
                    dateSplitStarts, 
                    dateSplitEnds)

sink(file.path(base.path,
               "console_output",
               paste0("qgmm_results_",
                      panelistSelection,
                      ".txt")), 
     type=c("output", "message"))

# Source Kaplan code
source(file.path(base.path, "EIS/code/gmmq.R"))

# Loop over types of Rates
for (ii in 1:length(channelTypes)) {
  
channel <- channelTypes[ii]

# Read in channel
estimationDataFileName <- 
  file.path(base.path, 
            "csv_output",
            channel, 
            paste0("estimation_data_", 
                   panelistSelection, 
                   ".csv"))

estimationData <- read_csv(estimationDataFileName)

  # Loop over data splits
  # Currently there is nothing to loop since we 
  # are looking at ALL periods
  for (jj in 1:nrow(dateSplits)) {
    
    timePeriod <- dateSplits[jj, "period"]
    startDate <- dateSplits[jj, "dateSplitStarts"]
    endDate <- dateSplits[jj, "dateSplitEnds"]
    
    estimationDataSubset <- 
      estimationData %>%
      filter(DATE >= startDate 
             & DATE <= endDate)
    
    ################################################################################
    #### Setup Data #####
    ################################################################################
    
    # Outcome Variable
    Y <- as.matrix(estimationDataSubset$Y)
    n <- nrow(Y)
    
    # Columns of 1s (for intercept)
    X.excl <- matrix(data=1, nrow=n, ncol=1)
    # Dependent Variable
    D <- as.matrix(eval(parse(text="estimationDataSubset$X_TB")))

    # Instruments
    Z1.formula <- as.formula(paste0("Y~Z1+Z2_TB+Z3"))
    Z.inst1 <- lm(Z1.formula, data=estimationDataSubset)$fitted
    Z2.formula <- as.formula(paste0("X_TB~Z1+Z2_TB+Z3"))
    Z.inst2 <- lm(Z2.formula, data=estimationDataSubset)$fitted
    
    # Combine Instruments
    Z.excl <- cbind(Z.inst1, Z.inst2)
    # Exogenous Variables
    Z <- cbind(Z.excl, X.excl)
    # All RHS variables
    X <- cbind(D, X.excl)
    
    # Pooled Panel to get starting point
    PLM.formula <- as.formula(paste0("Y~X_TB|Z1+Z2_TB+Z3"))
    PLM <- plm(PLM.formula, 
               data=estimationDataSubset, 
               model='pooling', 
               index=c('HOUSEHOLD', 'DATE'))
    
    cat("================================================================================\n")
    cat(paste0("OLS Estimation for '", panelistSelection, "' panelists on '", channel, "' \n"))
    cat("================================================================================\n")
    
    print(summary(PLM))
    StartingPointReg <- c(PLM$coef[2], 
                          PLM$coef[1])
    
    ################################################################################
    ##### Setup Functions #####
    ################################################################################
    
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
    Ldfn.gmmq <- function(y,x,b) - cbind(y[,-1],x)
    Lfn2b.gmmq <- function(y,x,b) - Lfn.gmmq(y,x,b) #-y[,1]+cbind(x,y[,-1])%*%b #log-linear, 1-tau
    Ldfn2b.gmmq <- function(y,x,b) - Ldfn.gmmq(y,x,b) #cbind(x,y[,-1])
    
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

    ################################################################################
    ##### Run QGMM #####
    ################################################################################
    
    # Initialize  Variables
    dimX <- ncol(X)
    H.HUGE <- 0.0001
    
    # Desired quantiles
    tau<-seq(0.1,0.9,0.1)
    nt<-length(tau)
    
    # Initialize parameters
    coef.beta<-array(0,dim=c(nt,1))
    coef.eis<-array(0,dim=c(nt,1))

    se.beta<-array(0,dim=c(nt,1))
    se.eis<-array(0,dim=c(nt,1))
    band.eis<-array(0,dim=c(nt,1))
    
    # SE band
    band<-c(0.05,0.95)
    nb<-length(band)
    
    # Loop through quantiles
    for (i in 1:nt){
      
      # GMMQ Function
      ret2b <- 
        tryCatch(gmmq(tau=tau[i], dB=dimX, Y=cbind(Y,D), 
                      X=X.excl, Z.excl=Z.excl,
                      Lambda=Lfn2b.gmmq, Lambda.derivative=Ldfn2b.gmmq,
                      h=H.HUGE, VERBOSE=FALSE, RETURN.Z=FALSE, 
                      b.init=StartingPointReg), 
                 error=function(w)list(b=c(NA,NA),h=NA))
      
      # Get Coefficients 
      coef.beta[i]<-conv2.fn(ret2b$b)[1]
      coef.eis[i]<-conv2.fn(ret2b$b)[2]
      
      # Calculate g.theta
      g.theta1<-1/(coef.beta[i]*coef.eis[i])
      g.theta2<--log(coef.beta[i])*(1/coef.eis[i]^2)
      g.theta<-c(g.theta1,g.theta2)
      
      # Initalize SE for tau
      se.beta.t<-array(0,dim=c(nb,1))
      se.eis.t<-array(0,dim=c(nb,1))
      
      for (j in 1:nb){
        
        # band<-bandwidth.rq(tau[i],n)
        # print(band[j])
        
        # Estimate covariance
        cov.est <- 
          cov.est.fn(tau=tau[i],Y=Y,X=X,Z=Z.excl,Lambda=Lfn2b,
                     Lambda.derivative=Ldfn2b,beta.hat=ret2b$b,
                     Itilde=Itilde.KS17,Itilde.deriv=Itilde.deriv.KS17,
                     h=H.HUGE,structure=c('ts'),cluster.X.col=0,
                     LRV.kernel=c('Bartlett'),LRV.ST=NA,
                     VERBOSE=FALSE,h.adj=band[j])
        
        # If empty, replace with NA
        if (is.na(cov.est)) {
          se.beta.t[j] <- NA
          se.eis.t[j] <- NA
        } 
        # Else, calculate SE
        else {
          cov <- cov.est
          
          cov_beta <- g.theta%*%cov%*%g.theta
          cov_eis <- cov[2,2]
          
          se.beta.t[j] <- sqrt(cov_beta/n)
          se.eis.t[j] <- sqrt(cov_eis/n)
        }
      }
      
      # Take the minimum form the band
      MinLoc <- which.min(se.eis.t)
      finalband <- band[MinLoc]
      min.se.beta <- se.beta.t[MinLoc]
      min.se.eis <- se.eis.t[MinLoc]
      
      # Final SEs
      se.beta[i] <- min.se.beta
      se.eis[i] <- min.se.eis
      band.eis[i] <- finalband
    }
    
    # Combine Results
    QGMMResults <- 
      as.data.frame(
        cbind(tau,
              coef.beta,
              se.beta,
              coef.eis,
              se.eis,
              band.eis))
                                       
    colnames(QGMMResults) <- 
      c("tau", 
        "Beta", 
        "Beta.SE", 
        "EIS", 
        "EIS.SE", 
        "EIS.Band")
    
    cat("================================================================================\n")
    cat(paste0("Quantile Estimation for '", panelistSelection, "' panelists on '", channel, "' \n"))
    cat("================================================================================\n")
    
    print(QGMMResults)
    
    # Output Results
    # outputFileName <- file.path(base.path,
    #                             "console_output",
    #                             paste0(panelistSelection,
    #                                    ".csv"))
    #                                       
    # write_csv(QGMMResults, outputFileName)
    
  }
}
