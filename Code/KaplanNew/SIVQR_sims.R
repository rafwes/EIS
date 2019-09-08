# simulations for de Castro, Galvao, and Kaplan (201X)
# "Smoothed IV quantile regression, with estimation of quantile Euler equations"
# Question? Comments? Need help? kaplandm@missouri.edu
# This and other code at: faculty.missouri.edu/~kaplandm

rm(list=ls()) #clear workspace
SAVE.FLAG <- TRUE ##########
PARALLEL <- 4 # Fastest (for me): PARALLEL>1, power=Balanced
if (SAVE.FLAG) { NREP <- 1000; BREP <- 1 } else { NREP <- 3; BREP <- 1 } #BREP=299; 8min/rep (DGP1) if BREP=399, n=1e4. n=1e4,BREP=100,NREP=200 should be ~3days, but (bootstrap) simulation error is much higher, not very informative.
TAUs <- 1:2/4
NS <- c(20,50,200,500) #est: c(20,50,200,500)  inf: c(1e2,1e3,1e4)
overall.start.time <- Sys.time()

# Load the code; file(s) must be in current directory or path
source("gmmq.R")
require('quantreg')

# PARALLEL setup and error checking
if (PARALLEL>1) {
  if (!require("parallel") || !require("foreach")) {warning("Install package foreach in order to run PARALLEL."); PARALLEL <- 1}
  if (!require("doParallel")) {warning("Install package doParallel in order to run PARALLEL."); PARALLEL <- 1}
  PARALLEL <- tryCatch({workers <- makeCluster(PARALLEL); registerDoParallel(workers); on.exit(stopCluster(workers),add=TRUE); PARALLEL}, error=function(Err){warning(sprintf("Error creating %d clusters",PARALLEL)); 1})
  clusterSetRNGStream(workers,112358)
} else if (PARALLEL<0) {
  warning("PARALLEL must be a non-negative integer: 0 or 1 for not parallel, positive for number of parallel CPUs to use.")
}

OUTFILE.RMSE1 <- paste0(sprintf("SIVQR_sims_RMSE1_NREP%d_",NREP),
                        format(Sys.time(),"%Y_%m_%d"),".txt")
OUTFILE.robRMSE1beta <- paste0(sprintf("SIVQR_sims_robustRMSE1beta_NREP%d_",NREP),
                           format(Sys.time(),"%Y_%m_%d"),".txt")
OUTFILE.robRMSE1gamma <- paste0(sprintf("SIVQR_sims_robustRMSE1gamma_NREP%d_",NREP),
                           format(Sys.time(),"%Y_%m_%d"),".txt")
OUTFILE.RMSE2 <- paste0(sprintf("SIVQR_sims_RMSE2_NREP%d_",NREP),
                        format(Sys.time(),"%Y_%m_%d"),".txt")
OUTFILE.robRMSE2 <- paste0(sprintf("SIVQR_sims_robustRMSE2_NREP%d_",NREP),
                           format(Sys.time(),"%Y_%m_%d"),".txt")
OUTFILE.inf <- paste0(sprintf("SIVQR_sims_inf_NREP%d_BREP%d_",NREP,BREP),
                      format(Sys.time(),"%Y_%m_%d"),".txt")
if (!SAVE.FLAG) OUTFILE.RMSE1 <- OUTFILE.robRMSE1beta <- OUTFILE.robRMSE1gamma <- OUTFILE.RMSE2 <- OUTFILE.robRMSE2 <- OUTFILE.inf <- ""

# random functions drawing (Y,X,Z.excl)
rfn.JTPA1 <- function(n,beta.fn) {
  if (missing(n)) stop("n missing from rfn()")
  Z.excl <- 0 + (runif(n)>0.5)
  U <- runif(n)
  D <- 0 + (Z.excl & 0.75*runif(n)<U)
  # Y <- 60 + X*(U-0.5)*100 + qchisq(U,3)
  Y1 <- rowSums(t(beta.fn(U))*cbind(1,D))
  return(list(Y=cbind(Y1,D),X=array(1,dim=c(n,1)),Z.excl=matrix(Z.excl,nrow=n)))
}
rfn.IV.TS.general <- function(n,beta.fn,NORM.ERR) {
  #like p. 15 of https://eml.berkeley.edu/~mcfadden/e240b_f01/ch4.pdf
  RHO.Z <- 0.5
  if (NORM.ERR) {
    RHO.EPS <- 0.5
    Vs <- rnorm(n+1,0,sqrt(1-RHO.EPS^2))
  } else {  #Cauchy? see Table 2 here...http://robjhyndman.com/papers/ar1.pdf so C(L,1) innovations lead to C(mu,1/(1-RHO)) marginal distribution...check https://en.wikipedia.org/wiki/Cauchy_distribution#Transformation_properties so solve 1+RHO*s=s or s=1/(1-RHO) as in Table 2. Instead if innovations are C(0,t), then solve t+RHO*s=s so s=t/(1-RHO), set t=1-RHO
    RHO.EPS <- 0.5
    Vs <- (1-RHO.EPS) * rt(n+1,df=1,ncp=0)
  }
  Etas <- rnorm(n+1,0,sd=1) #was: 0.3
  Y1 <- D <- Z <- eps <- rep(NA,n+1)
  Z[1] <- rnorm(1)
  if (NORM.ERR) eps[1] <- rnorm(1) else eps[1] <- rt(1,df=1)
  D[1] <- Z[1] + Etas[1]
  for (t in 2:(n+1)) {
    Z[t] <- RHO.Z*Z[t-1] + rnorm(1,0,sqrt(1-RHO.Z^2))
    eps[t] <- RHO.EPS*eps[t-1] + Vs[t]
    D[t] <- Z[t] + Etas[t]
  }
  if (NORM.ERR) {
    Y1 <- rowSums(t(beta.fn(pnorm(eps)))*cbind(1,Z))
  } else {
    Y1 <- rowSums(t(beta.fn(pt(eps,df=1)))*cbind(1,Z))
  }
  return(list(Y=cbind(Y1[2:(n+1)],D[2:(n+1)]),X=array(1,dim=c(n,1)),Z.excl=matrix(D[1:n],nrow=n)))
}
rfn.IV.TS1 <- function(n,beta.fn) rfn.IV.TS.general(n,beta.fn,NORM.ERR=TRUE)
rfn.IV.TS2 <- function(n,beta.fn) rfn.IV.TS.general(n,beta.fn,NORM.ERR=FALSE)

# H.ADJS <- c(1,1/3,1/5,1/7) #MUST start with 1
# H.ADJS.STR <- c('1','1/3','1/5','1/7')
# H.ADJS.nSTR <- c('1','2/3','4/5','6/7')
# H.ADJ.INF.INDEX <- 3
H.ADJS <- c(1,1/5) #MUST start with 1
H.ADJS.STR <- c('1','1/5')
H.ADJS.nSTR <- c('1','4/5')
H.ADJ.INF.INDEX <- 2

DGPs <- list(list(DGPid="1",outfmt=1,structure='iid',cluster.X.col=NA, h.init.fn=function(tau,n) 0.0001, #tau^(-2)/n
                  rfn=rfn.JTPA1,ns=NS,taus=TAUs,dB=2, #ns=20,50,500...need 1e4 for Wald to be good.
                  beta.fn=function(tau)rbind(60+qchisq(tau,3),(tau-0.5)*100),
                  Lambda=function(y,x,b) y[,1]-x*b[1]-y[,2]*b[2],
                  Lambda.derivative=function(y,x,b) -cbind(x,y[,2])),
             list(DGPid="2",outfmt=1,structure='ts',cluster.X.col=NA, h.init.fn=function(tau,n) 0.0001, #tau^(-2)/n
                  rfn=rfn.IV.TS1,ns=NS,taus=TAUs,dB=2,
                  beta.fn=function(tau)rbind(qnorm(tau),1), #NOT (tau-0.5)*4 b/c can have x<0
                  Lambda=function(y,x,b) y[,1]-x*b[1]-y[,2]*b[2],
                  Lambda.derivative=function(y,x,b) -cbind(x,y[,2])),
             list(DGPid="3",outfmt=1,structure='ts',cluster.X.col=NA, h.init.fn=function(tau,n) 0.0001, #tau^(-2)/n
                  rfn=rfn.IV.TS2,ns=NS,taus=TAUs,dB=2,
                  beta.fn=function(tau)rbind(qt(tau,df=1),1), #NOT (tau-0.5)*4 b/c can have x<0
                  Lambda=function(y,x,b) y[,1]-x*b[1]-y[,2]*b[2],
                  Lambda.derivative=function(y,x,b) -cbind(x,y[,2])))


if (!SAVE.FLAG) { ######################################################
  # DGPs <- DGPs[1]
  # DGPs <- list(list(DGPid="1",outfmt=1,structure='iid',cluster.X.col=NA, h.init.fn=function(tau,n) 0.0001, #tau^(-2)/n
  #           rfn=rfn.JTPA1, ns=c(1e3,1e4), taus=c(0.5), dB=2, 
  #           beta.fn=function(tau)rbind(60+qchisq(tau,3),(tau-0.5)*100),
  #           Lambda=function(y,x,b) y[,1]-x*b[1]-y[,2]*b[2],
  #           Lambda.derivative=function(y,x,b) -cbind(x,y[,2])))
  H.ADJS <- H.ADJS[c(1,H.ADJ.INF.INDEX)]
  H.ADJS.STR <- H.ADJS.STR[c(1,H.ADJ.INF.INDEX)]
  H.ADJS.nSTR <- H.ADJS.nSTR[c(1,H.ADJ.INF.INDEX)]
  H.ADJ.INF.INDEX <- 2
}


cat(sprintf("\\begin{table}[htbp]\n\\centering
\\caption{\\label{tab:sim-RMSE1}Simulation results for estimators' RMSE, $\\num{%d}$ replications. ``SIVQR'' is the estimator in \\cref{eqn:def-beta-hat}; ``QR'' is quantile regression (no IV); ``IV'' is the usual (conditional mean) IV estimator.}
\\begin{tabular}{cccrcrrrcrrr}
\\toprule
 &   &    &       & & \\multicolumn{3}{c}{Robust RMSE} & & \\multicolumn{3}{c}{Median Bias} \\\\
\\cmidrule{6-8}\\cmidrule{10-12}
DGP & $\\tau$ & Parameter & $n$ 
& & SIVQR & QR & IV 
& & SIVQR & QR & IV \\\\
\\midrule\n", NREP),
    file=OUTFILE.RMSE1,sep='',append=TRUE)

cat("\\begin{table}[htbp]\n\\centering
\\caption{\\label{tab:sim-robust-RMSE1gamma}Simulated precision of estimators of $\\gamma_\\tau$.}
\\sisetup{round-precision=2}\n\\begin{threeparttable}\n\\begin{tabular}{ccrcrrrcrrr}
\\toprule
&    &       & & \\multicolumn{3}{c}{Robust RMSE} & & \\multicolumn{3}{c}{Median Bias} \\\\
\\cmidrule{5-7}\\cmidrule{9-11}
DGP & $\\tau$ & $n$ 
    & & SIVQR & QR & IV 
    & & SIVQR & QR & IV \\\\
\\midrule\n",
    file=OUTFILE.robRMSE1gamma,sep='',append=TRUE)

cat("\\begin{table}[htbp]\n\\centering
\\caption{\\label{tab:sim-robust-RMSE1gamma}Simulated precision of estimators for $\\beta_\\tau$.}
\\sisetup{round-precision=2}\n\\begin{threeparttable}\n\\begin{tabular}{ccrcrrrcrrr}
\\toprule
&    &       & & \\multicolumn{3}{c}{Robust RMSE} & & \\multicolumn{3}{c}{Median Bias} \\\\
\\cmidrule{5-7}\\cmidrule{9-11}
DGP & $\\tau$ & $n$ 
    & & SIVQR & QR & IV 
    & & SIVQR & QR & IV \\\\
\\midrule\n",
    file=OUTFILE.robRMSE1beta,sep='',append=TRUE)

cat(sprintf("\\begin{table}[htbp]\n\\centering
\\caption{\\label{tab:sim-robust-RMSE2}Simulation results for SIVQR estimators' ``robust RMSE'' (replacing bias with median bias, replacing standard deviation with IQR) with different bandwidths; $\\num{%d}$ replications. $h_0$ is the smallest numerically feasible bandwidth.}
\\begin{tabular}{cccrc%2$sc%2$s}
\\toprule
&   &    &       & & \\multicolumn{%3$d}{c}{Robust RMSE} & & \\multicolumn{%3$d}{c}{Median Bias} \\\\
\\cmidrule{6-8}\\cmidrule{10-12}
DGP & $\\tau$ & Parameter & $n$ 
            & & $h_0$ & %4$s  
            & & $h_0$ & %4$s \\\\
\\midrule\n", NREP, paste0(rep('r',length(H.ADJS)),collapse=''), length(H.ADJS), paste0(sprintf("$h\\propto n^{-%s}$",H.ADJS.STR[-1]), collapse=' & ')),
    file=OUTFILE.robRMSE2,sep='',append=TRUE)

cat(sprintf("\\begin{table}[htbp]
\\caption{\\label{tab:sim-ttest}Simulated size of two-sided tests of $H_0:\\gamma_\\tau=\\gamma_0$.}
\\centering\n\\begin{threeparttable}\n\\begin{tabular}{ccrrlrrr}
\\toprule
DGP & $\\tau$ & \\multicolumn{1}{c}{$n$} & \\multicolumn{1}{c}{$\\alpha$} 
& & \\multicolumn{1}{c}{Wald} & \\multicolumn{1}{c}{BS-$t$} & \\multicolumn{1}{c}{BS} \\\\
\\midrule\n"),
    file=OUTFILE.inf,sep='',append=TRUE)

for (iDGP in 1:length(DGPs)) {
  DGP <- DGPs[[iDGP]]
  cat(sprintf("DGP %s\n",DGP$DGPid),
      file="",sep='',append=TRUE)
  starttime <- Sys.time()
  cat(format(Sys.time())); cat('\n00000')
  dB <- length(DGP$beta.fn(0.5))
  tmp1 <- array(NA,dim=c(NREP,dB,length(DGP$taus),length(DGP$ns),length(H.ADJS)))
  tmp2 <- array(NA,dim=c(NREP,dB,length(DGP$taus),length(DGP$ns)))
  beta.hats <- list(QR=tmp2, IV=tmp2, GMM=tmp1)
  W.hats <- array(NA,dim=c(NREP,length(DGP$taus),length(DGP$ns))) #,length(H.ADJS)
  tmp <- array(NA,dim=c(NREP,length(DGP$taus),length(DGP$ns))) #2nd param only, H.ADJ=H.ADJS[H.ADJ.INF.INDEX] only
  pvals.ttest <- list(W=tmp,BSt=tmp,BS=tmp)
  # n0frac <- array(NA,dim=c(NREP,length(DGP$ns)))
  set.seed(112358) #for replicability
  for (irep in 1:NREP) {
    cat(sprintf("\b\b\b\b\b%05d",irep))
    # DRAW DATA
    dat <- DGP$rfn(max(DGP$ns),DGP$beta.fn)
    # RUN/SAVE EST
    for (itau in 1:length(DGP$taus)) {
      if (PARALLEL>1) {
        # bp.raw <- NULL #for non-parallel: uncomment
        # for (i in 1:length(DGP$ns)) { #for non-parallel: uncomment
        bp.raw <- foreach(i=1:length(DGP$ns),.combine=cbind,.inorder=TRUE) %dopar% { #for non-parallel: comment
          n <- DGP$ns[i];  tau <- DGP$taus[itau]
          beta.hats.par <- matrix(NA,nrow=dB,ncol=length(H.ADJS))
          Y <- matrix(dat$Y[1:n,],nrow=n)
          X <- matrix(dat$X[1:n,],nrow=n)
          Z.excl <- matrix(dat$Z.excl[1:n,],nrow=n)
          gmmq.ret <- tryCatch(gmmq(tau=tau,dB=dB,Y=Y,X=X,Z.excl=Z.excl,
                 Lambda=DGP$Lambda, Lambda.derivative=DGP$Lambda.derivative,
                 h=DGP$h.init.fn(tau,n), VERBOSE=FALSE, RETURN.Z=TRUE),
                 error=function(w){print(sprintf("gmmq error in rep%d\n",irep));NA})
          if (is.na(gmmq.ret[1])) {
            matrix(rep(NA,4+prod(dim(beta.hats.par))), ncol=1)
          } else {
            h0 <- gmmq.ret$h
            L <- DGP$Lambda(y=Y, x=X, b=gmmq.ret$b)
              # apply(cbind(Y,X),1,function(x)DGP$Lambda(x[1:dim(Y)[2]],x[-(1:dim(Y)[2])],gmmq.ret$b))
            n0 <- sum(abs(L)<h0)
            # n0frac[irep,i] <- n0/n
            for (iadj in 1:length(H.ADJS)) {
              if (iadj>1) {
                h.adj <- h0*(n/n0)*n^(-H.ADJS[iadj])
                if (iadj==H.ADJ.INF.INDEX) { #try Kato (2012) ROT, page 264
                  sum1 <- 1 + mean(Y[,2]^2) + mean(Z.excl^2) + mean(Y[,2]^2*Z.excl^2)
                  sum2 <- 1 + mean(Y[,2])^2 + mean(Z.excl)^2 + mean(Y[,2]*Z.excl)^2
                  AlphaTau <- (1-qnorm(tau))^2 * dnorm(qnorm(tau))
                  h.adj <- n^(-1/5) * (4.5*sum1/(AlphaTau*sum2))^(1/5)
                }
                gmmq.ret <- tryCatch(gmmq(tau,dB,Y,X,Z.excl,
                                 DGP$Lambda, DGP$Lambda.derivative,
                                 h=h.adj,VERBOSE=FALSE,RETURN.Z=TRUE,b.init=beta.hats.par[,1]), error=function(w){cat(sprintf("gmmq error in rep%d\n",irep));NA})
              }
              if (is.na(gmmq.ret[1])) {
                beta.hats.par[,iadj] <- WaldZ <- Waldh <- NA
              } else {
                beta.hats.par[,iadj] <- gmmq.ret$b
                if (iadj==H.ADJ.INF.INDEX) {
                  WaldZ <- gmmq.ret$Z;  Waldh <- gmmq.ret$h
                }
              }
            }
            if (is.na(WaldZ[1])) {
              pval.W <- pval.BSt <- pval.BS <- NA
            } else {
              Wald.ret <- 
                gmmq.wald.test(a.hat=beta.hats.par[2,H.ADJ.INF.INDEX]-DGP$beta.fn(tau)[2], A.hat=matrix(c(0:1,rep(0,dB-2)),nrow=1), 
                               tau=tau, Y=Y, X=X, Z=WaldZ, 
                               Lambda=DGP$Lambda,Lambda.derivative=DGP$Lambda.derivative, 
                               beta.hat=beta.hats.par[,H.ADJ.INF.INDEX], 
                               Itilde=Itilde.KS17, Itilde.deriv=Itilde.deriv.KS17, 
                               h=Waldh, 
                               structure=DGP$structure, cluster.X.col=NA, LRV.kernel='Bartlett', LRV.ST=NA, VERBOSE=FALSE, h.adj=1)
              W.hat <- Wald.ret$Wald.stat
              pval.W <- Wald.ret$pval
              pval.BSt <- pval.BS <- NA
              if (BREP>1) {
                pval.BSt <- gmmq.BS.test(a.fn=function(b,b0)b[2]-b0[2], A.fn=function(b,b0)matrix(c(0:1,rep(0,dB-2)),nrow=1), tau=tau, Y=Y, X=X, Z=WaldZ, Z.excl=Z.excl, Lambda=DGP$Lambda, Lambda.derivative=DGP$Lambda.derivative, beta.hat=beta.hats.par[,H.ADJ.INF.INDEX], beta0=DGP$beta.fn(tau), Itilde=Itilde.KS17, Itilde.deriv=Itilde.deriv.KS17, h=Waldh, structure=DGP$structure, cluster.X.col=DGP$cluster.X.col, LRV.kernel='Bartlett', LRV.ST=NA, VERBOSE=FALSE, h.adj=1, BREP=BREP, BLAG=NA)$pval
                pval.BS <- gmmq.BS2.test(T.fn=function(b)b[2], tau=tau, Y=Y, X=X, Z=WaldZ, Z.excl=Z.excl, Lambda=DGP$Lambda, Lambda.derivative=DGP$Lambda.derivative, beta.hat=beta.hats.par[,H.ADJ.INF.INDEX], beta0=DGP$beta.fn(tau), Itilde=Itilde.KS17, Itilde.deriv=Itilde.deriv.KS17, h=Waldh, structure=DGP$structure, cluster.X.col=DGP$cluster.X.col, VERBOSE=FALSE, h.adj=1, BREP=BREP, BLAG=NA)$pval
              }
            }
            slice <- c(W.hat, pval.W, pval.BSt, pval.BS, c(beta.hats.par))
            # bp.raw <- cbind(bp.raw,slice) #for non-parallel: uncomment
            matrix(slice,ncol=1) #for non-parallel: comment out
          }
        }
        W.hats[irep,itau,] <- bp.raw[1,]
        pvals.ttest$W[irep,itau,] <- bp.raw[2,]
        pvals.ttest$BSt[irep,itau,] <- bp.raw[3,]
        pvals.ttest$BS[irep,itau,] <- bp.raw[4,]
        tmp <- matrix(bp.raw[-(1:4),],nrow=dB)
        tmp <- tmp[,c(matrix(1:ncol(tmp),nrow=ncol(bp.raw),byrow=TRUE))]
        beta.hats$GMM[irep,,itau,,] <- tmp
      } else stop("For non-parallel, modify code by uncommenting and commenting the lines as indicated in the current code comments (search for 'non-parallel').")
      # if (DGP$outfmt==1) {
        for (i in 1:length(DGP$ns)) {
          n <- DGP$ns[i];  tau <- DGP$taus[itau]
          suppressWarnings(beta.hats$QR[irep,,itau,i] <- rq(formula=Y~X,tau=tau,data=data.frame(Y=dat$Y[1:n,1],X=dat$Y[1:n,2]))$coefficients)
          tmpZ <- cbind(dat$X[1:n,],dat$Z.excl[1:n,])
          beta.hats$IV[irep,,itau,i] <- solve(t(tmpZ)%*%cbind(1,dat$Y[1:n,2]))%*%t(tmpZ)%*%dat$Y[1:n,1]
        }
      # }
    }
  }
  cat("\n")
  beta0s <- DGP$beta.fn(DGP$taus)
  tmp <- array(NA,dim=c(dim(t(beta0s)),length(DGP$ns))) #dim(beta.hats[[1]])[-1]
  tmp2 <- as.list(NULL); tmpn <- as.character(NULL)
  for (i in 1:length(H.ADJS)) { tmp2 <- c(tmp2,list(tmp)); tmpn <- c(tmpn,sprintf("GMM%d",i)) }
  tmp2 <- c(list(tmp),list(tmp),tmp2);  tmpn <- c("QR","IV",tmpn)
  names(tmp2) <- tmpn
  est.avgs <- est.medians <- biases <- median.biases <- SEs <- IQRs <- RMSEs <- robust.RMSEs <- avg.abs.devs <- tmp2 
  for (k in 1:length(est.avgs)) {
    BH <- beta.hats[[min(3,k)]];  if (k>2) BH <- array(BH[ , , , ,k-2],dim=dim(BH)[-5])
    est.avgs[[k]] <- apply(BH,2:4,mean,na.rm=TRUE)
    est.medians[[k]] <- apply(BH,2:4,median,na.rm=TRUE)
    biases[[k]] <- est.avgs[[k]] - 
      array(rep(beta0s,length(DGP$ns)),
            dim=c(dim(beta0s),length(DGP$ns)))
    median.biases[[k]] <- est.medians[[k]] - 
      array(rep(beta0s,length(DGP$ns)),
            dim=c(dim(beta0s),length(DGP$ns)))
    SEs[[k]] <- apply(BH,2:4,sd,na.rm=TRUE)
    IQRs[[k]] <- apply(BH,2:4,IQR,na.rm=TRUE)
    RMSEs[[k]] <- sqrt(biases[[k]]^2+SEs[[k]]^2)
    robust.RMSEs[[k]] <- sqrt(median.biases[[k]]^2+(IQRs[[k]]/1.349)^2)
    beta0reps <- array(rep(array(rep(beta0s,each=NREP),
                                 dim=c(NREP,dim(beta0s))),
                           length(DGP$ns)),
                       dim=dim(BH))
    avg.abs.devs[[k]] <- apply(abs(BH-beta0reps),2:4,mean,na.rm=TRUE)
  }
  # if (DGP$outfmt==2) {
#     for (ip in 1:dB) {
#       for (i in 1:length(DGP$ns)) {
      # }
    # }
  # } else if (DGP$outfmt==1) {
    for (itau in 1:length(DGP$taus)) {
      for (ip in 1:dB) {
        for (i in 1:length(DGP$ns)) {
          cat(sprintf("%-20s & $%4s$ & $\\num{%6d}$ && $\\num{%7.2f}$ & $\\num{%7.2f}$ & $\\num{%7.2f}$ && $\\num{%7.2f}$ & $\\num{%7.2f}$ & $\\num{%7.2f}$ \\\\\n",
                  ifelse(i==1 && itau==1, # && ip==1
                         sprintf("%s",DGP$DGPid)," "),
                  ifelse(i==1,sprintf("%4.2f",DGP$taus[itau])," "), # && ip==1
                  # ifelse(i==1,ifelse(ip==1,"$\\beta_\\tau$","$\\gamma_\\tau$")," "), 
                  DGP$ns[i],
                  robust.RMSEs$GMM1[ip,itau,i],
                  robust.RMSEs$QR[ip,itau,i],
                  robust.RMSEs$IV[ip,itau,i],
                  median.biases$GMM1[ip,itau,i], 
                  median.biases$QR[ip,itau,i], 
                  median.biases$IV[ip,itau,i]), 
          file=ifelse(ip==1,OUTFILE.robRMSE1beta,OUTFILE.robRMSE1gamma),
          sep='',append=TRUE)
          #
#           cat(sprintf("%-20s & %6s & $\\num{%6d}$ && $\\num{%7.2f}$ & $\\num{%7.2f}$ && $\\num{%7.2f}$ & $\\num{%7.2f}$ \\\\\n",
#                       ifelse(i==1 && ip==1,sprintf("%s",DGP$DGPid)," "),
#                       ifelse(i==1,ifelse(ip==1,"$\\beta$","$\\gamma$")," "), #sprintf("%d",ip)
#                       DGP$ns[i],
#                       RMSEs$GMM[ip,1,i],biases$GMM[ip,1,i], #,SEs[ip,1,i]
#                       RMSEs$GMM[ip,2,i],biases$GMM[ip,2,i]), #,SEs[ip,2,i]
#               file=OUTFILE.RMSE2,sep='',append=TRUE)
          #
          tmpRMSE <- tmpBias <- NULL
          for (ih in 1:length(H.ADJS)) {
            tmpRMSE <- c(tmpRMSE,robust.RMSEs[[ih+2]][ip,itau,i])
            tmpBias <- c(tmpBias,median.biases[[ih+2]][ip,itau,i])
          }
          cat(sprintf("%-20s & $%4s$ & %6s & $\\num{%6d}$ && %s && %s \\\\\n",
                      ifelse(i==1 && ip==1 && itau==1, sprintf("%s",DGP$DGPid)," "),  ifelse(i==1 && ip==1,sprintf("%4.2f",DGP$taus[itau])," "),  ifelse(i==1,ifelse(ip==1,"$\\beta_\\tau$","$\\gamma_\\tau$")," "),  DGP$ns[i],
                      paste0(sprintf("$\\num{%7.2f}$",tmpRMSE),collapse=' & '),   paste0(sprintf("$\\num{%7.2f}$",tmpBias),collapse=' & ')), 
              file=OUTFILE.robRMSE2,sep='',append=TRUE)
        }
      }
    }
  # } else stop(sprintf("DGP$outfmt==%d?!",DGP$outfmt))

  for (itau in 1:length(DGP$taus)) {
    for (i in 1:length(DGP$ns)) {
      for (ALPHA in c(0.10)) {
        cat(sprintf("%-20s & $%4s$ & $\\num{%6d}$ & $\\num{%4.2f}$ && $\\num{%5.3f}$ & $\\num{%5.3f}$ & $\\num{%5.3f}$ \\\\\n",
                    ifelse(i==1 && itau==1,
                           sprintf("%s",DGP$DGPid)," "),
                    ifelse(i==1,sprintf("%4.2f",DGP$taus[itau])," "),
                    DGP$ns[i], ALPHA, 
                    mean(pvals.ttest$W[,itau,i]<ALPHA,na.rm=TRUE),
                    mean(pvals.ttest$BSt[,itau,i]<ALPHA,na.rm=TRUE),
                    mean(pvals.ttest$BS[,itau,i]<ALPHA,na.rm=TRUE)), 
            file=OUTFILE.inf,sep='',append=TRUE)
      }
    }
  }
  
  if (!exists("SAVE.FLAG")) SAVE.FLAG <- TRUE

  # RMSE graph
  GRAY1 <- '#666666';  GRAY2 <- '#CCCCCC';  LEGROOM <- 1.25
  # if (DGP$outfmt==2) {
    if (SAVE.FLAG) pdf(file=sprintf("SIVQR_sims_RMSE2_h0_DGPid%s_NREP%d_%s.pdf", DGP$DGPid,NREP,format(Sys.time(),"%Y_%m_%d")),
                       pointsize=12, width=7, height=7)
    par(family="serif",mar=c(5.0,6.0,6.0,2.1))
    plot(1:2,0:1,type="n",log='x', xlab="n",ylab="RMSE",
         xlim=range(DGP$ns), ylim=c(0,LEGROOM*max(unlist(RMSEs[-(1:2)]))), 
         mgp=c(3,1,0), main="", cex.main=2, cex.lab=2, cex.axis=2)
    LWD <- 4
    lines(DGP$ns,RMSEs$GMM1[1,1,], col=8, lty=2, lwd=LWD) #ip,tau,n
    lines(DGP$ns,RMSEs$GMM1[2,1,], col=1, lty=2, lwd=LWD)
    if (length(DGP$taus)>1) {
      lines(DGP$ns,RMSEs$GMM1[1,2,], col=8, lty=1, lwd=LWD)
      lines(DGP$ns,RMSEs$GMM1[2,2,], col=1, lty=1, lwd=LWD)
    }
    legend('topright',c(sapply(1:min(2,length(DGP$taus)),function(i){
      as.expression(substitute(hat(beta)(s),list(s=DGP$taus[i])))}),
      sapply(1:min(2,length(DGP$taus)),function(i){
        as.expression(substitute(hat(gamma)(s),list(s=DGP$taus[i])))})),
      ncol=2,lwd=5,col=c(8,8,1,1),lty=c(2,1,2,1),inset=0,bty='n',cex=1.8, x.intersp=0.5, y.intersp=1.2)
    if (SAVE.FLAG) dev.off()
  # } else if (DGP$outfmt==1) {
    if (SAVE.FLAG) pdf(file=sprintf("SIVQR_sims_RMSE1_h0_DGPid%s_NREP%d_%s.pdf", DGP$DGPid,NREP,format(Sys.time(),"%Y_%m_%d")),
                       pointsize=12, width=7, height=7)
    par(family="serif",mar=c(5.0,6.0,6.0,2.1))
    plot(1:2,0:1,type="n",log='x',
         xlab="n",ylab="RMSE",
         xlim=range(DGP$ns), 
         ylim=c(0,LEGROOM*max(c(max(RMSEs$GMM1[2,1:min(2,length(DGP$taus)),]),max(RMSEs$QR[2,1:min(2,length(DGP$taus)),]),max(RMSEs$IV[2,1:min(2,length(DGP$taus)),])),na.rm=TRUE)), mgp=c(3,1,0),
         main="", cex.main=2, cex.lab=2, cex.axis=2)
    LWD <- 4
    lines(DGP$ns,RMSEs$IV[2,1,], col=GRAY2, lty=2, lwd=LWD+2) #ip,tau,n
    lines(DGP$ns,RMSEs$QR[2,1,], col=GRAY1, lty=2, lwd=LWD+1) 
    lines(DGP$ns,RMSEs$GMM1[2,1,], col='#000000', lty=2, lwd=LWD) 
    if (length(DGP$taus)>1) {
      lines(DGP$ns,RMSEs$IV[2,2,], col=GRAY2, lty=1, lwd=LWD+2) 
      lines(DGP$ns,RMSEs$QR[2,2,], col=GRAY1, lty=1, lwd=LWD+1) #ip,tau,n
      lines(DGP$ns,RMSEs$GMM1[2,2,], col='#000000', lty=1, lwd=LWD) #ip,tau,n
    }
    RMSE.legtext <- c(sprintf("SIVQR(%4.2f)",DGP$taus[1]),
                 sprintf("SIVQR(%4.2f)",DGP$taus[2]),
                 sprintf("QR(%4.2f)",DGP$taus[1]),
                 sprintf("QR(%4.2f)",DGP$taus[2]),
                 sprintf("IV(%4.2f)",DGP$taus[1]),
                 sprintf("IV(%4.2f)",DGP$taus[2]))
    if (length(DGP$taus)==1) legtext <- c(sprintf("SIVQR(%4.2f)",DGP$taus[1]), sprintf("QR(%4.2f)",DGP$taus[1]), sprintf("IV(%4.2f)",DGP$taus[1]))
    legend('topright', RMSE.legtext,
      ncol=3,lwd=3,col=rep(c('#000000',GRAY1,GRAY2),each=2),
      lty=rep(2:1,3),inset=0,bty='n',cex=1.3, 
      x.intersp=0.5, y.intersp=1.2, text.width=rep(c(2/7,1/4,1/4),each=length(DGP$taus)))
    if (SAVE.FLAG) dev.off()
  # } else stop(sprintf("DGP$outfmt==%d?!",DGP$outfmt))
  
  # Robust RMSE graph
  # if (DGP$outfmt==2) {
    if (SAVE.FLAG) pdf(file=sprintf("SIVQR_sims_robustRMSE2_h0_DGPid%s_NREP%d_%s.pdf", DGP$DGPid,NREP,format(Sys.time(),"%Y_%m_%d")),
                       pointsize=12, width=7, height=7)
    par(family="serif",mar=c(5.0,6.0,6.0,2.1))
    plot(1:2,0:1,type="n",log='x',
         xlab="n",ylab="Robust RMSE",
         xlim=range(DGP$ns), 
         ylim=c(0,LEGROOM*max(unlist(robust.RMSEs[-(1:2)]))), mgp=c(3,1,0),
         main="", cex.main=2, cex.lab=2, cex.axis=2)
    LWD <- 4
    lines(DGP$ns,robust.RMSEs$GMM1[1,1,], col=8, lty=2, lwd=LWD) #ip,tau,n
    lines(DGP$ns,robust.RMSEs$GMM1[2,1,], col=1, lty=2, lwd=LWD)
    if (length(DGP$taus)>1) {
      lines(DGP$ns,robust.RMSEs$GMM1[1,2,], col=8, lty=1, lwd=LWD)
      lines(DGP$ns,robust.RMSEs$GMM1[2,2,], col=1, lty=1, lwd=LWD)
    }
    legend('topright',c(sapply(1:min(2,length(DGP$taus)),function(i){
      as.expression(substitute(hat(beta)(s),list(s=DGP$taus[i])))}),
      sapply(1:min(2,length(DGP$taus)),function(i){
        as.expression(substitute(hat(gamma)(s),list(s=DGP$taus[i])))})),
      lwd=5,col=c(8,8,1,1),lty=c(2,1,2,1),inset=0,bty='n',cex=1.8, x.intersp=0.5, y.intersp=1.2)
    if (SAVE.FLAG) dev.off()
  # } else if (DGP$outfmt==1) {
    if (SAVE.FLAG) pdf(file=sprintf("SIVQR_sims_robustRMSE1_h0_DGPid%s_NREP%d_%s.pdf", DGP$DGPid,NREP,format(Sys.time(),"%Y_%m_%d")),
                       pointsize=12, width=7, height=7)
    par(family="serif",mar=c(5.0,6.0,6.0,2.1))
    plot(1:2,0:1,type="n",log='x',
         xlab="n",ylab="Robust RMSE",
         xlim=range(DGP$ns), 
         ylim=c(0,LEGROOM*max(c(max(robust.RMSEs$GMM1[2,1:min(2,length(DGP$taus)),]),max(robust.RMSEs$QR[2,1:min(2,length(DGP$taus)),]),max(robust.RMSEs$IV[2,1:min(2,length(DGP$taus)),])),na.rm=TRUE)), 
         mgp=c(3,1,0), main="", cex.main=2, cex.lab=2, cex.axis=2)
    LWD <- 4
    lines(DGP$ns,robust.RMSEs$IV[2,1,], col=GRAY2, lty=2, lwd=LWD+2) 
    lines(DGP$ns,robust.RMSEs$QR[2,1,], col=GRAY1, lty=2, lwd=LWD+1) 
    lines(DGP$ns,robust.RMSEs$GMM1[2,1,], col='#000000', lty=2, lwd=LWD) 
    if (length(DGP$taus)>1) {
      lines(DGP$ns,robust.RMSEs$IV[2,2,], col=GRAY2, lty=1, lwd=LWD+2)
      lines(DGP$ns,robust.RMSEs$QR[2,2,], col=GRAY1, lty=1, lwd=LWD+1)
      lines(DGP$ns,robust.RMSEs$GMM1[2,2,], col='#000000', lty=1, lwd=LWD)
    }
    legend('topright', RMSE.legtext, 
           # c(sprintf("SIVQR(%4.2f)",DGP$taus[1]),
           #   sprintf("SIVQR(%4.2f)",DGP$taus[2]),
           #   sprintf("QR(%4.2f)",DGP$taus[1]),
           #   sprintf("QR(%4.2f)",DGP$taus[2]),
           #   sprintf("IV(%4.2f)",DGP$taus[1]),
           #   sprintf("IV(%4.2f)",DGP$taus[2])),
           ncol=3,lwd=3,col=rep(c('#000000',GRAY1,GRAY2),each=2),
           lty=rep(2:1,3),inset=0,bty='n',cex=1.3, #was: cex=1.8
           x.intersp=0.5, y.intersp=1.2, 
           text.width=rep(c(2/7,1/4,1/4),each=length(DGP$taus)))
    if (SAVE.FLAG) dev.off()
  # } else stop(sprintf("DGP$outfmt==%d?!",DGP$outfmt))

  cat(format(Sys.time()-starttime),
      file="",sep='\n',append=TRUE)
}

nind <- 2
# dat <- beta.hats[[3]][,2,1,1,1]; hist(dat, freq=FALSE, breaks=15); lines(rep(beta0s[2],2),c(0,1e5),lwd=5); curve(dnorm(x, mean=mean(dat), sd=sd(dat)), col=2, lty=2, lwd=2, add=TRUE)
dat <- beta.hats[[3]][,2,1,nind,H.ADJ.INF.INDEX]; hist(dat, freq=FALSE, breaks=15); lines(rep(beta0s[2],2),c(0,1e5),lwd=5); curve(dnorm(x, mean=mean(dat), sd=sd(dat)), col=2, lty=2, lwd=2, add=TRUE)
# t2s <- W.hats[,1,1]; hist(t2s, freq=FALSE, breaks=15); curve(dchisq(x,df=1), col=2,lty=2,lwd=2,add=TRUE)
t2s <- W.hats[,1,nind]; hist(ifelse(abs(t2s)<15,t2s,-1), freq=FALSE, breaks=15); curve(dchisq(x,df=1), col=2,lty=2,lwd=2,add=TRUE)

cat("\\bottomrule\n\\end{tabular}\n\\end{table}",
    file=OUTFILE.RMSE1,sep='\n',append=TRUE)
cat(sprintf("\\bottomrule\n\\end{tabular}\n%%\n\\begin{tablenotes}
  \\item $\\num{%d}$ replications. ``SIVQR'' is the estimator in \\cref{eqn:def-beta-hat}; ``QR'' is quantile regression (no IV); ``IV'' is the usual (mean) IV estimator. 
\\end{tablenotes}\n\\end{threeparttable}
\\end{table}", NREP),
    file=OUTFILE.robRMSE1beta,sep='\n',append=TRUE)
cat(sprintf("\\bottomrule\n\\end{tabular}\n%%\n\\begin{tablenotes}
  \\item $\\num{%d}$ replications. ``SIVQR'' is the estimator in \\cref{eqn:def-beta-hat}; ``QR'' is quantile regression (no IV); ``IV'' is the usual (mean) IV estimator. 
\\end{tablenotes}\n\\end{threeparttable}
\\end{table}", NREP),
    file=OUTFILE.robRMSE1gamma,sep='\n',append=TRUE)
cat("\\bottomrule\n\\end{tabular}\n\\end{table}",
    file=OUTFILE.RMSE2,sep='\n',append=TRUE)
cat("\\bottomrule\n\\end{tabular}\n\\end{table}",
    file=OUTFILE.robRMSE2,sep='\n',append=TRUE)
cat(sprintf("\\bottomrule\n\\end{tabular}\n%%\n\\begin{tablenotes}
  \\item Nominal $\\alpha$ shown in table, $\\num{%d}$ replications. ``Wald'' uses \\cref{eqn:Wald-limit} directly, while ``BS-$t$'' bootstraps the Wald statistic and ``BS'' bootstraps the estimator, both as described in \\cref{sec:inf-testing-bootstrap} and using $\\num{%d}$ bootstrap draws each.
\\end{tablenotes}\n\\end{threeparttable}
\\end{table}", NREP, BREP),
    file=OUTFILE.inf,sep='\n',append=TRUE)
cat(paste0("% total elapsed time: ",format(Sys.time()-overall.start.time)),
    file=OUTFILE.inf,sep='\n',append=TRUE)
cat(paste0("% total elapsed time: ",format(Sys.time()-overall.start.time)),
    file=OUTFILE.robRMSE1gamma,sep='\n',append=TRUE)
cat(paste0("% total elapsed time: ",format(Sys.time()-overall.start.time)),
    file=OUTFILE.robRMSE2,sep='\n',append=TRUE)

if (PARALLEL>1) stopCluster(workers)

#EOF