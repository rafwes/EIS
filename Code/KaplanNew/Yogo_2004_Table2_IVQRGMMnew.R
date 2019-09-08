# "Replicate" (but with quantiles) Yogo (2004) Table 2 estimates of EIS
# See: de Castro, Galvao, Kaplan, and Liu (2018), "Smoothed GMM for quantile models"
# Code authors: Xin Liu and Dave Kaplan (kaplandm@missouri.edu)
# Runtime: 1-2 minutes

# NOTE: below, DATA.DIR and CODE.DIR must be modified to reflect your computer's directories. Also, package  GenSA  must be installed.

## DIRECTORY with data files
DATA.DIR <- "C:/Users/kaplandm/Box Sync/2018_dCGKL_quantile_GMM/_empirical/Yogo"

## LOAD CODE
CODE.DIR <- "https://faculty.missouri.edu/~kaplandm/code"
CODE.DIR <- "C:/Users/kaplandm/Box Sync/2018_dCGKL_quantile_GMM/Xin"
for (f in c("/gmmq.R","/ivqr_onestep.R","/ivqr_gmm.R")) source(paste0(CODE.DIR,f))
library("GenSA")
set.seed(112358)

## SETUP for output
SAVE.FLAG <- TRUE #TRUE: output to files; FALSE: display only in console
if (SAVE.FLAG) OUTFILE <- sprintf("Yogo_2004_Table2_quantile_GMM_%s.txt",format(Sys.time(),"%Y_%m_%d")) else OUTFILE <- ""
LOGLINEAR.FLAG <- TRUE #use log-linear (vs. nonlinear) model?

## PARAMETERS for simulated annealing: relatively short search time and very loose bounds
max.time <- 2 
upper <- rbind(100000,100000)
lower <- rbind(-100000,-100000)

## SETUP
COUNTRIES <- c("USA","UK","AUL","SWD") #data filenames
COUNTRY.ABBREVS <- c("US","UK","AUS","SWE") #official (ISO) abbrevs
TAUS <- 1:9/10
beta.ests <- gamma.ests <- beta.gmmq.ests <- gamma.gmmq.ests <- array(NA,dim=c(length(TAUS)+1,length(COUNTRIES)))
hs.used <- list()

st <- Sys.time()
H.TINY <- 0.0001;  H.HUGE <- 100;  H.MID <- 0.01
H <- H.TINY #which bandwidth to use

for (icountry in 1:length(COUNTRIES)) {
  
  COUNTRY <- COUNTRIES[icountry]
  cat(sprintf("Starting country %s\n",COUNTRY), file="")
  
  ## LOAD DATA
  # Downloaded from https://sites.google.com/site/motohiroyogo/research/EIS_Data.zip
  #(1)date,(2)r,(3)dp,(4)rf,(5)inf,(6)dc,(7)rr,(8)rrf,(9-12)z1,z2,z3,z4
  #(2) nominal stock return, (3) log dividend-price ratio
  # (4) nominal risk-free rate, (5) inflation rate, (6) log consumption growth,
  # (7) real stock return, (8) real risk-free return (interest rate)
  # (9) 2-quarter-lagged dp, (10) 2Q-lag rf, (11) 2Q-lag inf, (12) 2Q-lag dc

  dat <- read.table(file=paste0(DATA.DIR,sprintf("/%sQ.txt",COUNTRY)),header=TRUE,sep="",na.strings=c("NA",".")) # "Q"=quarterly data

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

  
  # Initialize storage variables
  tmp <- matrix(NA,ncol=length(TAUS),nrow=length(b.2sls))
  b.hats.GMM <- b.hats.MM <- data.frame(tmp) 
  names(b.hats.GMM) <- names(b.hats.MM) <- c(sprintf("tau%d",10*TAUS)) 
  hs.used[[COUNTRY]] <- data.frame(GMM=rep(NA,length(TAUS)), MM=NA)
  # 
  for (itau in 1:length(TAUS)) {
    cat(sprintf("Starting itau=%d of %d\n",itau,length(TAUS)), file="")
    tau <- TAUS[itau]

    if (LOGLINEAR.FLAG) {
      # log-linear, flipped (to 1-tau) to preserve interpretation of tau from quantile utility maximization
      # GMM
      ret <- ivqr.gmm(tau=tau,  Y=Y, X.excl=X.excl, Z.excl=Z.excl, D=D, h=H, dB=2,
                      max.time=max.time, upper=upper, lower=lower, structure='ts', LRV.kernel='QS',
                      Lambda=Lfn2b, Lambda.derivative = Ldfn2b,
                      Lambda.gmmq=Lfn2b.gmmq, Lambda.derivative.gmmq=Ldfn2b.gmmq,
                      b.init=rbind(1/6,0))
      b.hats.GMM[,which(TAUS==tau)] <- conv2.fn(ret$b)
      hs.used[[COUNTRY]][which(TAUS==tau),'GMM'] <- ret$h
      # 
      # MM
      ret.gmmq <- tryCatch(gmmq(tau=tau, dB=2, Y=cbind(Y,D), X=X.excl, Z.excl=Z.excl,
                                Lambda=Lfn2b.gmmq, Lambda.derivative=Ldfn2b.gmmq,
                                h=H, VERBOSE=FALSE, RETURN.Z=FALSE, b.init=rbind(1/6,0)),
                           error=function(w)list(b=c(NA,NA),h=NA))
      if (!is.na(ret.gmmq$b[1])[1]) { 
        b.hats.MM[,which(TAUS==tau)] <- conv2.fn(ret.gmmq$b) 
        hs.used[[COUNTRY]][which(TAUS==tau),'MM'] <- ret.gmmq$h 
      }
    } else { #nonlinear
      # GMM
      ret <- tryCatch(ivqr.gmm(tau=tau,  Y=Y, X.excl=X.excl, Z.excl=Z.excl, D=D, h=H, dB=2,
                               max.time=max.time, upper=upper, lower=lower, structure='ts', LRV.kernel='QS',
                               Lambda=Lfn2, Lambda.derivative = Ldfn2,
                               Lambda.gmmq=Lfn2.gmmq, Lambda.derivative.gmmq=Ldfn2.gmmq,
                               b.init=rbind(1,6)),
                      error=function(w)list(b=c(NA,NA),h=NA))
      if (!is.na(ret$b[1])[1]) {
        b.hats.GMM[,which(TAUS==tau)] <- conv3.fn(ret$b)
        hs.used[[COUNTRY]][which(TAUS==tau),'GMM'] <- ret$h 
      }
      # 
      # MM
      ret.gmmq <- tryCatch(gmmq(tau=tau, dB=2, Y=cbind(Y,D), X=X.excl, Z.excl=Z.excl,
                                Lambda=Lfn2.gmmq, Lambda.derivative=Ldfn2.gmmq,
                                h=H, VERBOSE=FALSE, RETURN.Z=FALSE, b.init=rbind(1,6)),
                           error=function(w)list(b=c(NA,NA),h=NA))
      if (!is.na(ret.gmmq$b[1])[1]) { 
        b.hats.MM[,which(TAUS==tau)] <- conv3.fn(ret.gmmq$b) 
        hs.used[[COUNTRY]][which(TAUS==tau),'MM'] <- ret.gmmq$h 
      } 
    } # END if-else (log-linear, nonlinear)
  }# END for loop over TAUS  
  
  beta.ests[1:length(TAUS), icountry] <- unlist(b.hats.GMM[1,])
  gamma.ests[1:length(TAUS), icountry] <- unlist(1/b.hats.GMM[2,])

  beta.gmmq.ests[1:length(TAUS), icountry] <- unlist(b.hats.MM[1,])
  gamma.gmmq.ests[1:length(TAUS), icountry] <- unlist(1/b.hats.MM[2,])

  beta.ests[length(TAUS)+1, icountry] <- beta.gmmq.ests[length(TAUS)+1, icountry] <- conv.fn(b.2sls)[1]
  gamma.ests[length(TAUS)+1, icountry] <- gamma.gmmq.ests[length(TAUS)+1, icountry] <- conv.fn(b.2sls)[2]

} # END for loop over countries



## OUTPUT tabular for LaTeX: beta for 4 countries.  (MM, GMM)
if (!exists("SAVE.FLAG")) {
  SAVE.FLAG <- TRUE
  OUTFILE <- sprintf("Yogo_2004_Table2_quantile_GMM_%s.txt",format(Sys.time(),"%Y_%m_%d"))
}
for (i in 0:1) {
  paramstr <- 'beta';  ests.GMM <- beta.ests;  ests.MM <- beta.gmmq.ests
  if (i==1) {
    paramstr <- 'gamma';  ests.GMM <- gamma.ests;  ests.MM <- gamma.gmmq.ests
  }
  cat(sprintf("\\sisetup{round-precision=%d,round-mode=places,table-format=%s}\n\\begin{table}[htbp]\n\\centering\n\\caption{\\label{tab:EIS-Yogo-%3$s}Smoothed MM and GMM estimates of $\\%3$s_\\tau$, %4$s model.}\n\\begin{threeparttable}\n", 2-i, ifelse(i==0,"1.2","-2.1"), paramstr, ifelse(LOGLINEAR.FLAG,"log-linear","nonlinear")), 
      file=OUTFILE,append=TRUE,sep='')
  cat(sprintf("\\begin{tabular}[c]{C%s}\n", paste0(rep(" BBB",length(COUNTRIES)),collapse='')), file=OUTFILE,append=TRUE,sep='')
  cat("\\toprule\n", file=OUTFILE, append=TRUE, sep='')
  cat(paste0(" & & ", paste0(sprintf(" \\multicolumn{2}{c}{%s} ",COUNTRY.ABBREVS),collapse='&&'), "\\\\\n",
             paste0(sprintf("\\cmidrule{%d-%d}",3*1:length(COUNTRIES),1+3*1:length(COUNTRIES)),collapse=''), '\n', collapse=''),
      file=OUTFILE,append=TRUE,sep='')
  tmp <- sprintf(" && {$\\hat{\\%1$s}_\\mathrm{MM}$} & {$\\hat{\\%1$s}_\\mathrm{GMM}$}", paramstr)
  cat(paste0('{$\\tau$}', paste0(rep(tmp,length(COUNTRIES)),collapse=''),
             "\\\\\n", collapse=''), file=OUTFILE,append=TRUE,sep='')
  cat("\\midrule\n", file=OUTFILE,append=TRUE,sep='')
  for (itau in 1:length(TAUS)) {
    tau <- TAUS[itau]
    cat(paste0(ifelse(0.699<tau && tau<0.801,"\\rowstyle{\\bfseries}\n",""),
               sprintf("%3.1f && ", TAUS[itau]),
               paste0(sprintf("%10.3f & %10.3f ",
                              ests.MM[itau,], 
                              ests.GMM[itau,]), 
                      collapse=' && '),
               "\\\\\n", collapse=''), 
        file=OUTFILE, append=TRUE, sep='')
  }
  
  cat(paste0("\\midrule\n{2SLS} && ",
             paste0(sprintf(sprintf("\\multicolumn{2}{c}{%%10.%df}",2-i), 
                            ests.GMM[1+length(TAUS),] ), 
                    collapse=' && '),
             "\\\\\n", collapse=''),
      file=OUTFILE,append=TRUE,sep='')
  cat("\\bottomrule\n\\end{tabular}\n%
% \\begin{tablenotes}
%  \\item Using quarterly data from \\citet{Yogo04}. Instruments (besides constant) are twice lagged: nominal interest rate, inflation, log consumption growth, and log dividend-price ratio, identical to Table 2 of \\citet{Yogo04}.  The smoothing bandwidth is (usually) $h=0.0001$.  
% \\end{tablenotes}
\\end{threeparttable}
\\end{table}\n\n\n", file=OUTFILE,append=TRUE,sep='')
}







  










# GRAPH estimates
for (ic in 1:length(COUNTRIES)) {
  FNAME <- paste0(sub(".txt","", OUTFILE),'_',COUNTRIES[ic],'.pdf')
  if (SAVE.FLAG) pdf(file=FNAME,pointsize=12, width=7, height=7)
  par(family='serif', mar=c(6.0,8.0,6.0,2.1))
  YLIM <- c(0,1.5)
  if (!(COUNTRIES[ic] %in% c('USA','UK'))) YLIM <- c(-0.3,1.5)
  plot(0:2/2, 0:2, main="",
       type='n', lwd=4, col=1, mgp=c(3.5,1,0),
       xlim=c(0,1), ylim=YLIM,
       xlab=expression(Quantile~index~(tau)),
       ylab="Estimate",
       cex.lab=2, cex.axis=2, cex.main=2)
  lines(TAUS, beta.ests[1:length(TAUS),ic], lwd=4, col=1, lty=1, pch=1, type='p', cex=2)
  lines(TAUS, 1/gamma.ests[1:length(TAUS),ic], lwd=4, col=1, lty=2, pch=2, type='p', cex=2)
  lines(0:1,rep(beta.ests[1+length(TAUS),ic],2), lwd=1, col=1, lty=1) #2SLS beta
  lines(0:1,rep(1/gamma.ests[1+length(TAUS),ic],2), lwd=1, col=1, lty=1) #2SLS EIS
  legend('topright',inset=c(-0.00,0.0),legend=c(expression(hat(beta)(tau)),expression(1/hat(gamma)(tau))),pch=1:2,lty=NA,lwd=4,col=1,cex=1.8, x.intersp=-0.0, y.intersp=1.0,ncol=2,text.width=c(0.17))#,bty='n') #
  if (SAVE.FLAG) dev.off()
}






# .tex for combined figures (two side-by-side)
for (ctys in list(c('USA','UK','US','UK'), c('AUL','SWD','AUS','SWE'))) {
  cat(sprintf("\n\n\\begin{figure}[htbp]\n\\centering\\hfill\n\\includegraphics[width=0.49\\textwidth,clip=true,trim=35 25 20 70]{Figures/%s}\n\\hfill\n\\includegraphics[width=0.49\\textwidth,clip=true,trim=35 25 20 70]{Figures/%s}\n\\hfill\\null\n\\caption{\\label{fig:EIS-Yogo-%s}Two-step GMM estimates for %s (left) and %s (right); flat lines are 2SLS.}\n\\end{figure}\n\n", 
              paste0(sub(".txt","", OUTFILE),'_',ctys[1],'.pdf'), 
              paste0(sub(".txt","", OUTFILE),'_',ctys[2],'.pdf'), 
              paste0(ctys[3],'-',ctys[4]),
              ctys[3], ctys[4] ),
      file=OUTFILE, append=TRUE, sep='')
}

hs.used

Sys.time() - st

#EOF