# Simulations for de Castro, Galvao, Kaplan, and Liu (2018)
# Compare two-step GMM with MM

rm(list=ls())
SAVE.FLAG <- TRUE # save to file?
NREPLIC <- 15 # number of replications/draws per simulation
if (SAVE.FLAG) NREPLIC <- 1000

# Load code
source("ivqr_onestep.R")
source("ivqr_see.R")
source("ivqr_gmm.R")

# library("GenSA")
library("MASS") # for mvrnorm()

# Set output file
OUTFILE <- ""
if (SAVE.FLAG) OUTFILE <- paste0("IVQR_GMM_sims_v11TS_",format(Sys.time(),"%Y_%m_%d"),".txt")

# Set parameters
NS <- c(50, 200) #sample size
if (SAVE.FLAG) NS <- c(50, 200, 500)
TAUS <- 2/4 #quantile index
if (SAVE.FLAG) TAUS <- 1:2/4
H <- 0.1 #bandwidth
DISCOUNT <- 0.99;  EIS <- 0.2
MAX.TIME.GENSA <- 1
LB.GENSA <- -10;  UB.GENSA <- 10 #lower/upper bounds for parameters
rfn <- function(n) {
  U.SD <- 2.0;  V.SD <- 2.0;  RHO.UV <- 0.8
  UV.COV <- matrix(c(U.SD^2, RHO.UV*U.SD*V.SD, RHO.UV*U.SD*V.SD, V.SD^2), ncol=2)
  UV <- mvrnorm(n, rep(0, 2), UV.COV)
  U <- as.matrix(UV[,1]) - qnorm(tau)
  V <- as.matrix(UV[,2])
  # ln.eps <- rnorm(n=n) - qnorm(1-tau)
  Y <- X <- rep(NA,n)
  RHO.Z <- c(0.2, 0.4, 0.6, 0.8)
  dZ <- 4 #instruments: dZ Gaussian AR(1) processes; independent if Zerr.rho=0
  Z.sd <- rep(1, dZ) / (1-RHO.Z^2)
  Z.cov <- diag(Z.sd^2)
  Zerr.cov <- diag(Z.sd^2*(1-RHO.Z^2))
  Zerr.rho <- 0.1
  for (i in 1:3) {
    for (k in (i+1):4) {
      Zerr.cov[i,k] <- Zerr.cov[k,i] <- 
        Zerr.rho * sqrt(Zerr.cov[i,i])*sqrt(Zerr.cov[k,k])
      Z.cov[i,k] <- Z.cov[k,i] <- Zerr.cov[i,k] / (1-RHO.Z[i]*RHO.Z[k])
    }
  }
  Z <- cbind(matrix(NA, nrow=n, ncol=dZ), 1)
  # Z[1,1:dZ] <- rnorm(n=dZ, mean=0, sd=Z.sd)
  Z[1,1:dZ] <- mvrnorm(n=1, mu=rep(0,dZ), Sigma=Z.cov)
  Z.beta <- rep(1,dZ+1)
  for (t in 1:n) {
    # X[t] <- sum(Z[t,]*Z.beta) + ln.eps[t]
    # tmp <- sum(Z[t,]^2*Z.beta) #*c(1, 1, Z[t,4], Z[t,4], 1)
    # X[t] <- tmp + ln.eps[t]
    Zb <- Z.beta*c(1, 1, 1, 1, 1)
    Zb <- Z.beta*c(1, 1, V[t], V[t], V[t])
    X[t] <- sum(Z[t,]*Zb) + V[t]
    # Y[t] <- tmp*EIS + log(DISCOUNT)*EIS
    Y[t] <- X[t]*EIS + log(DISCOUNT)*EIS + U[t]
    # Zerr <- rnorm(dZ,0,sqrt(Z.sd^2*(1-RHO.Z^2)))
    Zerr <- mvrnorm(n=1, mu=rep(0,dZ), Sigma=Zerr.cov)
    if (t<n) Z[t+1,1:dZ] <- RHO.Z*Z[t,1:dZ] + Zerr
  }
  return(list(ln.C.rat=Y, ln.R=X, Z.excl=Z[,1:dZ]))
}

# Output the parameter values
cat(sprintf(paste0("%% NREPLIC=%d, NS=%s, TAUS=%s, H=%g, DISCOUNT=%g, EIS=%g"),
    NREPLIC, paste0(sprintf("%d/",NS),collapse=''), paste0(sprintf("%g/",TAUS),collapse=''), H, DISCOUNT, EIS), 
    file=OUTFILE,sep="\n",append=TRUE)

if (SAVE.FLAG) {
  # 
  # LaTeX table output
  # 
  cat("\\begin{table}[htbp]
\\centering
\\caption{\\label{tab:sim-GMM}Simulated precision of smoothed estimators of EIS.}
\\sisetup{round-precision=3,round-mode=places,table-format=-1.3}
\\begin{threeparttable}
\\begin{tabular}{ccrcS[table-format=1.3]S[table-format=1.3]S[table-format=1.3]cSSS}
\\toprule
      &    &       & & \\multicolumn{3}{c}{Robust RMSE} & & \\multicolumn{3}{c}{Median Bias} \\\\
\\cmidrule{5-7}\\cmidrule{9-11}
&&&&&\\multicolumn{2}{c}{GMM} &&&\\multicolumn{2}{c}{GMM}\\\\
\\cmidrule{6-7}\\cmidrule{10-11}
DGP & $\\tau$ & $n$ 
& & {MM} & {(2s)} & {(ID)}
& & {MM} & {(2s)} & {(ID)} \\\\
\\midrule\n", file=OUTFILE, sep='', append=TRUE)
}

# Allocate storage variables
tmp <- data.frame(SEE=rep(NA,NREPLIC), MM=NA, OneStep=NA, GMM=NA, GMM.ID=NA, TSLS=NA)
b.hats <- list()
for (i in 1:length(NS)) b.hats[[i]] <- tmp
h.hats.see <- matrix(NA, nrow=NREPLIC, ncol=length(NS))

start.time <- Sys.time() #to see elapsed time

for (tau in TAUS) {
  
set.seed(112358) #for replication

cat("Loop=0000", file="") #to update progress
for (irep in 1:NREPLIC) {
  cat("\b\b\b\b"); cat(sprintf("%04d",irep)) #progress update
  
  n <- max(NS)  
  # Generate data
  dat <- rfn(n)
  Y <- dat$ln.C.rat
  Z.excl <- dat$Z.excl
  Z <- cbind(1, Z.excl)
  X <- cbind(1, dat$ln.R)
  dB <- dim(X)[2]
  
  for (i in 1:length(NS)) {
    n <- NS[i]
    # Compute estimators and store
    # 
    # Kaplan and Sun (2017), IVQR-SEE with "optimal" bandwidth (no h_in specified)
    ret.see <- ivqr_see(p = tau, Y = Y[1:n], X = X[1:n,], Z = Z[1:n,])
    b.hats[[i]]$SEE[irep] <- ret.see$b[2]
    h.hats.see[irep,i] <- ret.see$hhat
    # 
    # # MM and "one-step" GMM
    # ret.1s <- ivqr.onestep(Y=Y, X.excl=matrix(X[,1],ncol=1), Z.excl=Z.excl, D=matrix(X[,2],ncol=1), tau=tau, h=H, dB=dB, structure='ts')
    # b.hats$OneStep[irep] <- ret.1s$b.onestep[1]
    # b.hats$MM[irep] <- ret.1s$b.gmmq[1]
    # # 
    # GMM with identity weighting matrix
    ret.id <- ivqr.gmm(Y=Y[1:n], X.excl=matrix(X[1:n,1],ncol=1), Z.excl=Z.excl[1:n,], D=matrix(X[1:n,2],ncol=1), tau=tau, h=H, dB=dB, max.time=MAX.TIME.GENSA, upper=rep(UB.GENSA,dB), lower=rep(LB.GENSA,dB), structure='ts', LRV.kernel='QS', weight.mtx=diag(dim(Z)[2]))
    b.hats[[i]]$GMM.ID[irep] <- ret.id$b[1]
    # 
    # Two-step GMM
    ret.2s <- ivqr.gmm(Y=Y[1:n], X.excl=matrix(X[1:n,1],ncol=1), Z.excl=Z.excl[1:n,], D=matrix(X[1:n,2],ncol=1), tau=tau, h=H, dB=dB, max.time=MAX.TIME.GENSA, upper=rep(UB.GENSA,dB), lower=rep(LB.GENSA,dB), structure='ts', LRV.kernel='QS')
    b.hats[[i]]$GMM[irep] <- ret.2s$b[1]
    b.hats[[i]]$OneStep[irep] <- ret.2s$b.onestep[1]
    b.hats[[i]]$MM[irep] <- ret.2s$b.MM[1]
    # 
    # 2SLS
    PZ <- Z[1:n,] %*% solve(t(Z[1:n,])%*%Z[1:n,]) %*% t(Z[1:n,])
    b.2sls <- solve(t(X[1:n,])%*%PZ%*%X[1:n,]) %*% t(X[1:n,])%*%PZ%*%Y[1:n]
    b.hats[[i]]$TSLS[irep] <- b.2sls[2]
  }
} # END for loop over irep/NREPLIC

# which(abs(b.hats$GMM-EIS)>1) #in which iterations is GMM "way off"?
# sum(is.na(b.hats))

for(i in 1:length(NS)){

n <- NS[i]
# Compute results and save to OUTFILE
b.hats.mean <- colMeans(b.hats[[i]], na.rm=TRUE)
b.hats.bias <- b.hats.mean - EIS
b.hats.sd <- apply(X = b.hats[[i]], MARGIN = 2, FUN = sd, na.rm=TRUE)
b.hats.median.bias <- apply(X = b.hats[[i]], MARGIN = 2, FUN = median, na.rm=TRUE) - EIS
b.hats.RMSE <- sqrt(b.hats.bias^2 + b.hats.sd^2)
b.hats.IQR <- apply(X = b.hats[[i]], MARGIN = 2, FUN = quantile, probs=0.75, type=6, na.rm=TRUE) - apply(X = b.hats[[i]], MARGIN = 2, FUN = quantile, probs=0.25, type=6, na.rm=TRUE)
b.hats.rRMSE <- sqrt(b.hats.median.bias^2 + (b.hats.IQR/1.349)^2)

# Histograms
XLIM <- range(b.hats[[i]])
XLIM <- XLIM + 0.05*c(-1,1)*(XLIM[2]-XLIM[1])
for (j in 1:dim(b.hats[[i]])[2]) hist(b.hats[[i]][,j], main=names(b.hats[[i]])[j], xlim=XLIM, ylim=c(0,5/(XLIM[2]-XLIM[1])), freq=FALSE)
# hist(h.hats.see)
cat(sprintf("%5.2f",quantile(x=h.hats.see[,i], probs=c(0,0.5,1), type=6, na.rm=TRUE)), file="")


# paste0(sprintf("%s.mean=%g", names(b.hats), colMeans(b.hats, na.rm=TRUE)), collapse=',')

# cat(paste0(names(b.hats),collapse=' & '), file=OUTFILE,sep="\n",append=TRUE)
cat(sprintf('\n n=%d,tau=%g \n', n, tau))
cat(sprintf(paste0("Bias=%6.4f, RMSE=%6.4f, Median.Bias=%6.4f, rRMSE=%6.4f: %s"),
            b.hats.bias, b.hats.RMSE, b.hats.median.bias, b.hats.rRMSE, names(b.hats[[i]])),
    file="", sep="\n", append=TRUE)

if (SAVE.FLAG) {
  # 
  # LaTeX table output
  # 
  cat(sprintf("4 & %4.2f & \\numnornd{%6d} && %14.11f & %14.11f & %14.11f && %14.11f & %14.11f & %14.11f \\\\\n", tau, n, b.hats.rRMSE['MM'], b.hats.rRMSE['GMM'], b.hats.rRMSE['GMM.ID'], b.hats.median.bias['MM'], b.hats.median.bias['GMM'], b.hats.median.bias['GMM.ID']), file=OUTFILE, sep='', append=TRUE)
} # END if(SAVE.FLAG){}

} # END for(i in 1:length(NS)){}

} # END OF for (tau in TAUS) {}


if (SAVE.FLAG) {
  # 
  # LaTeX table output
  # 
  cat(sprintf("\\bottomrule
\\end{tabular}
%%
\\begin{tablenotes}
\\item $\\numnornd{%d}$ replications. ``MM'' is the estimator in \\cref{eqn:def-est-MM}; ``GMM(2s)'' is the estimator in \\cref{eqn:def-est-2s}; ``GMM(ID)'' is the estimator in \\cref{eqn:def-est-GMM} with identity weighting matrix. 
\\end{tablenotes}
\\end{threeparttable}
\\end{table}\n", NREPLIC), file=OUTFILE, sep='', append=TRUE)
} # END if(SAVE.FLAG){}


cat(sprintf("%% total time elapsed: %s\n\n\n",format(Sys.time()-start.time)), 
    file=OUTFILE, sep='', append=TRUE)

#EOF