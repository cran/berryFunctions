\name{extremeStat}
\alias{extremeStat}
\title{Extreme value statistics for flood risk estimation}
\description{Extreme value statistics for flood risk estimation.
\code{\link{extremeStatLmom}} is much more exhaustive!}
\usage{extremeStat(dat, ReturnYears=c(5,10,20,50), Kommastellen=1,
ylim=range(dat, finite=TRUE), xlim=range(j$ReturnPeriod, finite=TRUE),
LegendPos="bottomright", AddRegr=TRUE, plot=TRUE, PlotParam=FALSE,
ParamPosX=3, ParamPosY=5, main="yearly Discharge Extrema  /  Return Period",
ylab="Discharge HQ  [m^3/s]", xlab="Return Period RP  [a]", lwd=3, cex=2,
pch=20, cex.par=0.7, cex.leg=1, ...) }
\arguments{
  \item{dat}{Daten}
  \item{ReturnYears}{Jaehrlichkeiten, fuer die der Abfluss berechnet wird.. DEFAULT: c(5,10,20,50)}
  \item{Kommastellen}{Kommastellenanzahl der Abflusswerte. DEFAULT: 1}
  \item{ylim}{Y-Plotbereich (Standard ueber Daten). DEFAULT: range(dat, finite}
  \item{xlim}{X-Plotbereich. DEFAULT: range(j$ReturnPeriod, finite}
  \item{LegendPos}{Position der Legende. DEFAULT: "bottomright"}
  \item{AddRegr}{Zeichnen einer Ln-Regression (T/F). DEFAULT: TRUE}
  \item{plot}{soll ueberhaupt geplottet werden?. DEFAULT: TRUE}
  \item{PlotParam}{Einzeichnen der Parameter der Verteilungen (T/F). DEFAULT: FALSE}
  \item{ParamPosX}{bei PlotParam=T: Ort der Parameter (-10= links, 10= rechts). DEFAULT: 3}
  \item{ParamPosY}{ditto: (-10= unten, 10= oben). DEFAULT: 5}
  \item{main}{DEFAULT: "yearly Discharge Extrema  /  Return Period"}
  \item{ylab}{einige Graphikparameter. DEFAULT: "Discharge HQ  [m^3/s]"}
  \item{xlab}{zur Achsenbeschriftung. DEFAULT: "Return Period RP  [a]"}
  \item{lwd}{und zur Liniengestaltung. DEFAULT: 3}
  \item{cex}{Character EXpansion. DEFAULT: 2}
  \item{pch}{PointCHaracter. DEFAULT: 20}
  \item{cex.par}{Schriftgroesse Parameterliste. DEFAULT: 0.7}
  \item{cex.leg}{Schriftgroesse Legendeneintraege. DEFAULT: 1}
  \item{\dots}{und eventuelle weitere Graphikparameter}
}
\value{list with discharge and parameters}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011-2014}
\note{Development of this function is stopped. \code{\link{extremeStatLmom}} is the replacement.}
\seealso{\code{\link{extremeStatLmom}} has much more distributions.}
\examples{
JM <- c(61.5, 77.0, 37.0, 69.3, 75.6, 74.9, 43.7, 50.8, 55.6, 84.1, 43.6, 81.9,
60.1, 72.4, 61.6, 94.8, 82.6, 57.2,  63.1, 73.8, 51.3, 93.6, 56.9, 52.1, 40.4,
48.9, 113.6, 35.4, 40.1, 89.6, 47.8, 57.6, 38.9, 69.7, 110.8)

EWV.bach <- extremeStat(dat=JM)
# Im Graphikfenster sollte jetzt ein Plot sein...
EWV.bach
# Beispiele fuer Argumente, die ich in der Funktion eingebaut habe:
extremeStat(JM, ReturnYears=c(2,5,10,15,20,25,30,50,100), plot=FALSE) $ discharge
par(bg=8)
extremeStat(JM, PlotParam=TRUE, ParamPosX=2, ParamPosY=3)
extremeStat(JM, xlim=c(0,100), ylim=c(35,140), LegendPos=c(90,100))
par(bg="yellow")
extremeStat(JM, PlotParam=TRUE, LegendPos="right", AddRegr=FALSE)
#extremeStat(JM, lty=2)
}
\keyword{hplot}
\keyword{distribution}
\keyword{ts}
