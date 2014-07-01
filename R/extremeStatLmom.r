# Extreme value statistics for flood risk estimation
# based on extremeStat
# Berry Boessenkool, April/May 2014

# Input: Abfluss-jahresmaxima als Vektor
# Ausgabe: Abfluesse fuer vorgegebene Jaehrlichkeiten und/oder die Parameter der Verteilungen. Ausserdem wird eine Graphik erstellt.

# Siehe RclickHandbuch.wordpress.com Kapitel 15 fuer mehr Informationen zu den Rechenschritten.
# Das benoetigte Paket lmom (linear moments) wird bei Nichtvorhandensein installiert
# LiteraturStichwort: Wahrscheinlichkeitsfunktionen fuer die Extremwertstatistik


# L-Moments --------------------------------------------------------------------
extremeStatLmom <- function(
dat,                # Vector with discharge maxima
RPs=c(5,10,20,50),  # ReturnPeriods for which discharge is calculated
returnParam=FALSE,  # return distribution parameters instead of discharges (T/F)
gfProp=0.1,         # Proportion of highest values to compute Goodness of fit (RMSE) with
selection=1:13,     # selection of distributions, can be negative to leave some out
plot=TRUE,          # should a plot be created?
xlim=range(j[,2:3]),# x-limits. DEFAULT: xlim of plotting positions
ylim=NULL,          # DEFAULT: from min to extended max
col=rainbow(13),    # color for each distribution
main="Discharge Extrema  /  Return Period", # header of plot
xlab="Return Period RP  [a]", # x axis label
ylab="Discharge HQ  [m^3/s]",  # y axis label
lwd=2, # more graphical parameters
cex=1,
las=1,
lend=1,
pch=c(16,3),         # point characters for different plotting position methods
legarg=list(bty="o"),# list of arguments passed to legend except for legend, col, pch, lwd
... )                # further graphic parameter passed to plot, points and lines
{
# preparation ------------------------------------------------------------------
# load (and install, if necessary) package lmom:
if(!require(lmom)){install.packages("lmom"); require(lmom)}
# remove NAs:
dat <- dat[!is.na(dat)]
# Calculate Y-limits if not given
if(is.null(ylim)) ylim <- c(min(dat), max(dat)+0.1*diff(range(dat)))
# Calculate Plotting Positions according to Weibull:
m <- sort(dat) # ascendingly sorted extreme values
n <- length(m)
Rang <- 1:n
Pu.w <- Rang/(n+1) # empirical non-exceedence prob. (P_ne), alsmost = percentile
# Plotting Positions after Gringorten as in lmom:::evplot.default (rearranged):
Pu.g <- (Rang-0.44)/(n+0.12)
# RP = Returnperiod = recurrence interval = 1/P_exceedence = 1/(1-P_ne):
j <- data.frame(HQ1=m, RP.w=1/(1-Pu.w), RP.g=1/(1-Pu.g) )
# See chapter 15.2 RclickHandbuch.wordpress.com for differences in PP methods
# They're not used for fitting distributions, but for RMSE
#
# distributions ----------------------------------------------------------------
#  types, see ?lmom
#str(lmom:::lmom.dist)
di <- c("exp","gam","gev","glo","gno","gpa","gum","kap","ln3","nor","pe3","wak","wei")
distn <- c("exponential distribution","gamma","generalized extreme-value",
           "generalized logistic","generalized normal","generalized Pareto",
           "Gumbel (extreme-value type I)","kappa","three-parameter lognormal",
           "normal","Pearson type III","Wakeby","Weibull")
# Selection
if(any(abs(selection)>13)) stop("'selection' cannot be larger than 13.")
di <- di[selection]
distn <- distn[selection]
if(missing(col)) col <- col[selection] else
   if(length(col)==13)  col <- col[selection]
#
# L-Moments of sample
momente <- samlmu(dat, nmom=5)
# estimate the parameters of the underlying distributions from the l-moments
parameter <- sapply(di, function(d) try(get(paste0("pel",d))(momente), silent=TRUE))
# errors -----------------------------------------------------------------------
# check which Distributions could not be fitted
whicherror <- which(sapply(parameter, inherits, "try-error"))
if(length(whicherror)>0)
  {
  # format errors to output them as warnings at the end
  errors <- sapply(parameter[whicherror], function(x) strsplit(x[1], ":", fixed=TRUE)[[1]][2])
  errors <- gsub("  ", "", gsub("\n", "", errors))
    warning("The following distributions were omitted because lmom:::pelxxx failed:\n",
           paste(names(errors), errors, collapse="\n"))
  # remove erronous distributions from lists:
  di <- di[-whicherror]
  distn <- distn[-whicherror]
  col <- col[-whicherror]
  parameter <- parameter[-whicherror]
  }
# plot -------------------------------------------------------------------------
if(plot){
par(lend=lend)
# draw discharges over return periods:
plot(j$RP.w, j$HQ1, type="n", las=las, ylim=ylim, xlim=xlim, main=main,
     ylab=ylab, xlab=xlab, cex=cex, ...)
# range of discharges:
yval <- seq(from=par("usr")[3], to=par("usr")[4], length=500)
# add distributions:
for(i in 1:length(di))
  {
  pfunc <- get(paste0("cdf",di[i]))
  lines(1/(1-pfunc(yval,parameter[[i]])), yval, col=col[i], lwd=lwd, ...)
  }
points(j$RP.w, j$HQ1, pch=pch[1], cex=cex, ...)
if(length(pch)==1) pch[2] <-  if(pch[1]==3) 4 else 3 # 3 # pch[1]
points(j$RP.g,  j$HQ1, pch=pch[2], cex=cex, ...)
box()
# legend -----------------------------------------------------------------------
# write the names of distributions. - legargs: legend arguments
fixlegargs <- list(
  legend=c("Weibull plotting positions","Gringorten plotting positions",distn),
  pch=c( pch, rep(NA, length(distn))),
  lwd=c(NA,NA,rep(lwd,length(distn))),
  col=c(1,1,col),
  x="bottomright",
  cex=0.7)
# remove unchangable args specified by user (without warning):
userlegargs <- legarg[! names(legarg) %in% c("legend","pch","lwd","col") ]
# overwrite defaults and merge in user-supplied arguments:
fixlegargs[names(userlegargs)] <- userlegargs
# actually plot legend:
do.call(legend, args=fixlegargs)
} # end if plot
# output -----------------------------------------------------------------------
if(returnParam)  return(parameter)   else
{
discharge <- sapply(di, function(d) as.numeric(get(paste0("qua",d))(f=1-1/RPs,
                                     para=get(paste0("pel",d))(momente))  ))
# if length(RPs)==1, discharge is only a vector,
if(is.null(dim(discharge)))
     discharge <- as.matrix(discharge)        # so convert it to a matrix,
else discharge <- t(discharge)                # or else transpose it
# column names:
colnames(discharge) <- paste0("RP.", RPs)
# Goodness of fits by RMSE of  distributions compared to top gfProp % of PP:
# Function to compute Root Mean Square Error = RMSE:
rmse <- function(a,b) sqrt( sum((a-b)^2)/length(b) )
gfdat <- j[order(j$HQ1, decreasing=TRUE),]
gfdat <- gfdat[ 1:(gfProp*nrow(j)), ]
# RMSE compared to both methods of calculating plotting positions:
RMSE.w <- sapply(di, function(d) rmse(get(paste0("qua",d))(f=1-1/gfdat$RP.w,
                                     para=get(paste0("pel",d))(momente)),gfdat$HQ1))
RMSE.g <- sapply(di, function(d) rmse(get(paste0("qua",d))(f=1-1/gfdat$RP.g,
                                     para=get(paste0("pel",d))(momente)),gfdat$HQ1))
# More complex estimates of quality of fits:
# browseURL("http://chjs.soche.cl/papers/vol4n1_2013/ChJS-04-01-04.pdf")
# Kolmogorov-Smirnov test p-value:
## ks.test(dat, "pnorm", mean(dat), sd(dat))$p.value
## ks.test(dat, "cdfnor", parameter[["nor"]])$p.value # slightly different!
## ks.test(dat, "cdfgum", parameter[["gum"]])$p.value
kstp <- sapply(di, function(d)
        ks.test(dat, paste0("cdf",d), parameter[[d]])$p.value )
# Add to output:
discharge <- data.frame(discharge, RMSE.w, RMSE.g, kstp) # cbind
# finish the function
return(discharge)
} # end if not returnParam then discharge
} # end function -------------------------######################################

## Goodness of fits by RMSE of  distributions compared to top gfProp % of PP:
## This was an old method that takes the given Q. It is extremely sensitive to
## little Variations in Q, so I calculated RMSE the other way around (with calculated RP)
#gfdat <- j[order(j$HQ1, decreasing=TRUE),]
#gfdat <- gfdat[ 1:(gfProp*nrow(j)), ]
#gfdist <- sapply(1:length(di), function(i)
#                  1/( 1-get(paste0("cdf",di[i]))(gfdat$HQ1,parameter[[i]]) )   )
## RMSE compared to both methods of calculating plotting positions:
#RMSE.w <- sapply(1:ncol(gfdist), function(i) rmse(gfdist[,i], gfdat$RP.w) )
#RMSE.g <- sapply(1:ncol(gfdist), function(i) rmse(gfdist[,i], gfdat$RP.g) )
## Add to output:
#discharge <- cbind(discharge, RMSE.w, RMSE.g)

# ToDo: Include Bootstrapping with user-specified proportion left out

