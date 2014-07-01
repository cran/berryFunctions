\name{extremeStatLmom}
\alias{extremeStatLmom}
\title{Extreme value statistics for flood risk estimation}
\description{Extreme value statistics for flood risk estimation.
Fits 13 distributions based on linear moments.}
\usage{extremeStatLmom(dat, RPs=c(5, 10, 20, 50), returnParam=FALSE, gfProp=0.1,
 selection=1:13, plot=TRUE, xlim=range(j[, 2:3]), ylim=NULL, col=rainbow(13),
 main="Discharge Extrema  /  Return Period", xlab="Return Period RP  [a]",
 ylab="Discharge HQ  [m^3/s]", lwd=2, cex=1, las=1, lend=1, pch=c(16, 3),
 legarg=list(bty="o"), ...) }
\arguments{
  \item{dat}{Vector with discharge maxima}
  \item{RPs}{ReturnPeriods for which discharge is calculated. DEFAULT: c(5,10,20,50)}
  \item{returnParam}{return distribution parameters instead of discharges? (T/F). DEFAULT: FALSE}
  \item{gfProp}{Proportion of highest values to compute Goodness of fit (RMSE) with. DEFAULT: 0.1}
  \item{selection}{selection of distributions, can be negative to leave some out. DEFAULT: 1:13}
  \item{plot}{should a plot be created?. DEFAULT: TRUE}
  \item{xlim}{x-limits. DEFAULT: xlim of plotting positions. DEFAULT: range(j[,2:3])}
  \item{ylim}{DEFAULT: from min to extended max. DEFAULT: NULL}
  \item{col}{color for each distribution. DEFAULT: rainbow(13)}
  \item{main}{header of plot. DEFAULT: "Discharge Extrema  /  Return Period"}
  \item{xlab}{x axis label. DEFAULT: "Return Period RP  [a]"}
  \item{ylab}{y axis label. DEFAULT: "Discharge HQ  [m^3/s]"}
  \item{lwd}{more graphical parameters. DEFAULT: 2}
  \item{cex}{Character Expansion. DEFAULT: 1}
  \item{las}{Label Axis Style. DEFAULT: 1}
  \item{lend}{Line END type. DEFAULT: 1}
  \item{pch}{point characters for different plotting position methods. DEFAULT: c(16,3)}
  \item{legarg}{list of arguments passed to legend (except for legend, col, pch, lwd). DEFAULT: list(bty="o")}
  \item{\dots}{further graphic parameter passed to plot, points and lines}
}
\value{\emph{either} distribution parameters \emph{or} discharge for given return periods. The latter is a data.frame with the given RPs, the root mean square error (RMSE) for the top \code{gfProp} weibull and gringorton plotting positions as well as the p.values from a simple ks.test for each distribution function.}
\references{\url{http://RclickHandbuch.wordpress.com} Chapter 15 (German)\cr
Christoph Mudersbach: Untersuchungen zur Ermittlung von hydrologischen Bemessungsgroessen mit Verfahren der instationaeren Extremwertstatistik }
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2014}
\seealso{the predecessor \code{\link{extremeStat}}, the package lmom}
\examples{

# 0. Data from river in Austria
# 1. Basic Examples
# 2. Goodness of fit
# 2.1. Measures for GOF
# 2.2. weights based on GOF
# 2.3. weighted mean based on GOF
# 2.4. effect of data proportion used to estimate GOF
# 3. Parameters

# 0. Data from river in Austria ############################################

JM <- c(61.5, 77.0, 37.0, 69.3, 75.6, 74.9, 43.7, 50.8, 55.6, 84.1, 43.6, 81.9,
60.1, 72.4, 61.6, 94.8, 82.6, 57.2,  63.1, 73.8, 51.3, 93.6, 56.9, 52.1, 40.4,
48.9, 113.6, 35.4, 40.1, 89.6, 47.8, 57.6, 38.9, 69.7, 110.8)


# 1. Basic Examples ########################################################

extremeStatLmom(JM)

# Estimate discharge that empirically could occur every 50 years:
Q50 <- extremeStatLmom(JM, RPs=50)
sort(Q50[,1], decr=TRUE)
# 107 to 132 m^3/s can make a relevant difference in engineering!


# Line colors / selection distributions to be plotted:
extremeStatLmom(JM)
extremeStatLmom(JM, col=heat.colors(13))
extremeStatLmom(JM, col=heat.colors(13), selection=c(4,9,1))
extremeStatLmom(JM, col=1:3, selection=c(4,9,1))
extremeStatLmom(JM, legarg=list(cex=0.5, x="bottom", box.col="red", col=3))
# col is ignored. should be specified inside extremeStatLmom directly as vector



# 2. Goodness of fit ########################################################
# 2.1. Measures for GOF ----------------------------------------------------
# how well do the distributions fit the original data?
k <- extremeStatLmom(JM); k
pairs(k[,5:7]) # measures of goodness of fit are NOT really correlated
# I wish they were - apparently, it does matter which one is used

Ranks <- rank(k$RMSE.w)
Ranks <- cbind(Ranks, rank(k$RMSE.g))
Ranks <- cbind(Ranks, rank(1-k$kstp))
rownames(Ranks) <- rownames(k)
Ranks <- Ranks[ order(rowMeans(Ranks), decreasing=TRUE) , ]
plot(Ranks[,1], 1:13, type="o", ylab="", xlab="Rank", yaxt="n", 
     main="Ranking of distributions fitting the data best")
axis(2, 1:13, rownames(Ranks), las=2)
lines(Ranks[,2], 1:13, type="o", col=2, pch=2)
lines(Ranks[,3], 1:13, type="o", col=3, pch=3)
legend("topright", c("RMSE Weibull","RMSE gring.","KS-test P value"), 
       lty=1, col=1:3, pch=1:3)
# All three measures of goodness of fit indicate Kappa to be among the best,
# but normal is either middel, best, or worst fitting distribution function!

k <- extremeStatLmom(Nile); k
pairs(k[,5:7]) # a little better


# 2.2. weights based on GOF --------------------------------------
esJM <- extremeStatLmom(JM)
# normalized Goodness of Fit:
GF <- esJM[,"RMSE.w"] # the lower, the better, the more weight
GF <- max(GF)-GF#+mean(GF) # with mean added, the worst fit is not completely excluded
GF <- GF/sum(GF)
names(GF) <- rownames(esJM)
#
GF1 <- esJM[,"RMSE.w"]
GF1 <- max(GF1)-GF1+min(GF1) # with mean or min added, the worst fit is not completely excluded
GF1 <- GF1/sum(GF1)
#
plot(sort(GF1, decr=TRUE), ylim=c(0, 0.1), type="o", xaxt="n", ylab="Weight",
     las=1, xlab="Dist", main="Weight of distributions for average")
lines(sort(GF, decr=TRUE), pch=3, col=2, type="o")
axis(1, 1:13, names(sort(GF, decr=TRUE)), las=2)
legend("bottomleft", c("max(rw)-rw + min(rw)","max(rw)-rw"), col=1:2, pch=1:2, lty=1,
       title="Weighted by RMSE weibull plotting pos. (rw)")


# 2.3. weighted mean based on GOF --------------------------------------
extremeStatLmom(JM)
# range of discharges:
yval <- seq(from=par("usr")[3], to=par("usr")[4], length=500)
# add weighted average of distributions:

momente <- samlmu(JM, nmom=5)
parameter <- sapply(names(GF), function(d) get(paste0("pel",d))(momente))
CDFs <- sapply(names(GF), function(d) get(paste0("cdf",d))(yval,parameter[[d]]))

weightedMean <- CDFs[,1]*GF[1] + CDFs[,2]*GF[2] + CDFs[,3]*GF[3] + CDFs[,4]*GF[4] +
          CDFs[,5]*GF[5] + CDFs[,6]*GF[6] + CDFs[,7]*GF[7] + CDFs[,8]*GF[8] +
          CDFs[,9]*GF[9] + CDFs[,10]*GF[10] + CDFs[,11]*GF[11] +
          CDFs[,12]*GF[12] + CDFs[,13]*GF[13]

lines(1/(1-weightedMean), yval, lwd=3)
lines(1/(1-weightedMean), yval, col="white")

# Discharge estimated for 50 years return period, bei weighted average:
sum(GF * esJM[,"RP.50"]) # 117.2784


# 2.4. effect of data proportion used to estimate GOF ----------------
Goodness <- function(gfProp)
{
esJM <- extremeStatLmom(JM, gfProp=gfProp, plot=FALSE)
# normalized Goodness of Fit:
GF <- esJM[,"RMSE.w"] # the lower, the better, the more weight
GF <- max(GF)-GF#+mean(GF) # with mean added, the worst fit is not completely excluded
GF <- GF/sum(GF)           # plot(GF1, ylim=c(0, 0.1)); points(GF, pch=3)
# simple mean:              # plot(sort(GF))
av_simple <- mean(esJM[,"RP.50"])     # 116.8383
# Discharge estimated for 50 years return period, bei weighted average:
av_weight <- sum(GF * esJM[,"RP.50"]) # 117.2784
# mean of best 3 extreme value distribution functions:
av_3best <- mean(esJM[ order(esJM[,"RMSE.w"])[1:3] , "RP.50"]) # 118.3722
# most functions underestimate the discharge, if we assume that the weibull
# PP method correctly calculates the return period of the highest value
c(av_simple=av_simple, av_weight=av_weight, av_3best=av_3best)
}

Goodness(0.1)
Goodness(0.2)
Proportion <- seq(0.05, 1, len=100)
GoF <- sapply(Proportion, Goodness)
plot(Proportion, GoF[3,], type="l")
lines(Proportion, GoF[2,], col=2)
lines(Proportion, GoF[1,], col=3)
# The proportion of the data included for calculating RMSE does matter!



# 3. Parameters ##############################################################
# correlate parameters to other distribution functions:
p <- extremeStatLmom(JM, returnParam=TRUE)
p # p = Parameters for distributions of extreme values of discharge in small river
RPs <- c(5,10,20,50) # RPs = Return Periods that we want discharge estimated for

quagam(1-1/RPs, p$gam)
qgamma(1-1/RPs, shape=p$gam["alpha"], scale=p$gam["beta"])

quanor(1-1/RPs, p$nor)
qnorm(1-1/RPs, mean=p$nor["mu"], sd=p$nor["sigma"])

library("evd")
 quagum(1-1/RPs, p$gum)  # qgumbel from evd
qgumbel(1-1/RPs, loc=p$gum["xi"], scale=p$gum["alpha"])

            quaexp(1-1/RPs, p$exp)
p$exp["xi"] + qexp(1-1/RPs, rate=1/p$exp["alpha"])
# xi is location parameter, alpha is scale parameter, so 'rate' is 1/alpha

quagev(1-1/RPs, p$gev)  # ggev from evd.
  qgev(1-1/RPs, loc=p$gev["xi"], scale=p$gev["alpha"], shape=-p$gev["k"])
# evd's shape parameter has the opposite sign to lmom's

quagpa(1-1/RPs, p$gpa)  # ggpd from evd
  qgpd(1-1/RPs, loc=p$gpa["xi"], scale=p$gpa["alpha"], shape=-p$gpa["k"])
# evd's shape parameter has the opposite sign to lmom's

                  quawei(1-1/RPs, p$wei)
p$wei["zeta"] + qweibull(1-1/RPs, scale=p$wei["beta"], shape=p$wei["delta"])
# location parameter zeta shifts the quantiles

# qnweibull + qrweibull from evd
qnweibull(1-1/RPs, loc=p$wei["zeta"], scale=p$wei["beta"], shape=p$wei["delta"])
qrweibull(1-1/RPs, loc=p$wei["zeta"], scale=p$wei["beta"], shape=p$wei["delta"])
# evd's "reverse Weibull" is -x(1-F), where x(F) is the Weibull quantile function
# (with a change of sign for the location parameter)
-quawei(1/RPs, p$wei*c(-1,1,1))


                qualn3(1-1/RPs, p$ln3) # comparing 3param with 2param lognormal:
p$ln3["zeta"] + qlnorm(1-1/RPs, meanlog=p$ln3["mu"], sdlog=p$ln3["sigma"])

# evd also has the Frechet distribution, which is a special case of GEV
# (for negative values of lmom's shape parameter)
p$gev2 <- c(xi=43.2, alpha=28.3, k=-0.158)
  quagev(1-1/RPs, p$gev2)
qfrechet(1-1/RPs, loc =   p$gev2["xi"] + p$gev2["alpha"] / p$gev2["k"],
                  scale=               - p$gev2["alpha"] / p$gev2["k"],
                  shape=                             - 1 / p$gev2["k"])
}
\keyword{hplot}
\keyword{distribution}
\keyword{ts}
