# Extreme value statistics for flood risk estimation
# based on extremeStat
# Berry Boessenkool. Erstfassung 2011, Updates 2012 und 2014

# Input: Abfluss-jahresmaxima als Vektor
# Ausgabe: Abfluesse fuer vorgegebene Jaehrlichkeiten und/oder die Parameter der Verteilungen. Ausserdem wird eine Graphik erstellt.

# Das benoetigte Paket evd (ExtremeValueDistributions) wird bei Nichtvorhandensein installiert

# LiteraturStichwort: Wahrscheinlichkeitsfunktionen fuer die Extremwertstatistik

extremeStat <- function(
dat,         # Daten
ReturnYears=c(5,10,20,50), # Jaehrlichkeiten, fuer die der Abfluss berechnet wird.
Kommastellen=1,            # Kommastellenanzahl der Abflusswerte
ylim=range(dat, finite=TRUE),            # Y-Plotbereich (Standard ueber Daten)
xlim=range(j$ReturnPeriod, finite=TRUE), # X-Plotbereich
LegendPos="bottomright",                 # Position der Legende
AddRegr=TRUE,     # Zeichnen einer Ln-Regression (T/F)
plot=TRUE,        # soll ueberhaupt geplottet werden?
PlotParam=FALSE,  # Einzeichnen der Parameter der Verteilungen (T/F)
ParamPosX=3,      # bei PlotParam=T: Ort der Parameter (-10= links, 10= rechts)
ParamPosY=5,      #                  -"-               (-10= unten, 10= oben)
main="yearly Discharge Extrema  /  Return Period",
ylab="Discharge HQ  [m^3/s]",     # einige Graphikparameter
xlab="Return Period RP  [a]",    # zur Achsenbeschriftung
lwd=3,  # und zur Liniengestaltung
cex=2,
pch=20,
cex.par=0.7,                     # Schriftgroesse Parameterliste
cex.leg=1,                       # Schriftgroesse Legendeneintraege
... )                            # und eventuelle weitere Graphikparameter
#
# Hier fangen die Rechenvorschriften der Funktion an ---------------------------
#
{
if(!require(evd)){install.packages("evd"); require(evd)}
m <- sort(dat, decreasing=TRUE) ; Rang=1:length(m)
j <- data.frame(HQ1=m, ReturnPeriod=(length(m)+1)/Rang)
Ueber <- (ylim[2]-ylim[1])*.1    # Etwas ueber Graphikbereich hinausreichend
Q <-seq( (ylim[1]-Ueber) , (ylim[2]+Ueber) , len=1000 )
# Parameter der Verteilungen aus den Momenten der Daten bestimmen
NVmean  <- mean(j$HQ1)                                 #  Normalverteilung
NVsd    <-   sd(j$HQ1)                                 #  -"-
GAshape <- mean(j$HQ1)^2/var(j$HQ1)                    #  Gammaverteilung
GArate  <- mean(j$HQ1)/var(j$HQ1)                      #  -"-
GUloc   <- mean(j$HQ1)-0.577216*sd(j$HQ1)*sqrt(6)/pi   #  Gumbelverteilung
GUscale <-   sd(j$HQ1)*sqrt(6)/pi                      #  -"-
# Verteilungen ueber den Bereich Q (Graphikbereich an Abfluessen)
rpNV <- 1/( 1-   pnorm( Q, NVmean, NVsd )                 )
rpGA <- 1/( 1-  pgamma( Q, shape=GAshape, rate=GArate)    )
rpGU <- 1/( 1- pgumbel( Q,  loc=GUloc, scale=GUscale)     )
mod <- lm(j$HQ1~log(j$ReturnPeriod))                 # logarithmische Regression
kof <- coefficients(mod)  ; r2 <- summary(mod)$r.squared
#
# Ab hier wird die Ausgabe der Funktion vorbereitet ----------------------------
#
# Zuordnen der Parameter der Verteilungsfunktionen
parameter <- data.frame(Parameter=c("NORMAL mean","NORMAL sd",
                "GAMMA shape", "GAMMA rate","GUMBEL loc","GUMBEL m=1/scale",
                if (AddRegr==TRUE)c("LOG.REGR a", "LOG.REGR b", "LOG.REGR R^2")),
             Werte=c(NVmean,NVsd,GAshape,GArate,GUloc,1/GUscale,
                if (AddRegr==TRUE)c(kof[2],kof[1],r2)))
# Berechnen der Abfluesse fuer die gegebenen Jaehrlichkeiten
Zeilnamen <- c("Q Normalverteilung", "Q Gammaverteilung", "Q Gumbelverteilung",
          if (AddRegr)"Q lineareRegression")
Abfluesse <-  matrix(c( qnorm(1-(1/ReturnYears), NVmean, NVsd),
                       qgamma(1-(1/ReturnYears), shape=GAshape, rate=GArate),
                      qgumbel(1-(1/ReturnYears), loc=GUloc, scale=GUscale),
                      if (AddRegr) kof[2]*log(ReturnYears)+ kof[1]   ),
              byrow=TRUE, ncol=length(ReturnYears))
final <- round(Abfluesse, Kommastellen)
colnames(final) <- paste("RP", ReturnYears, "a", sep=".")
discharge <- data.frame(Jaehrlichkeiten=Zeilnamen, final)
#
# Ab hier wird die Graphik erstellt --------------------------------------------
if(plot){
# Zeichnen der Abfluesse ueber ihre Jaehrlichkeiten
plot(j$ReturnPeriod, j$HQ1, pch=pch, las=1, ylim=ylim, xlim=xlim, main=main,
     ylab=ylab, xlab=xlab, cex=cex, ...)
# Einlegen der Extremwertverteilungen
lines(rpNV, Q, col=2, lwd=lwd, ...)
lines(rpGA, Q, col=3, lwd=lwd, ...)
lines(rpGU, Q, col=4, lwd=lwd, ...)
RPx <- seq(0.01, xlim[2]+ 0.1*diff(xlim), len=1000)
if (AddRegr) lines(RPx, kof[2]*log(RPx)+kof[1], col=5, lwd=lwd, ...)
points(j$ReturnPeriod, j$HQ1, pch=pch, cex=cex, ...); box()
# Einfuegen der Legende
legend( if(mode(LegendPos)=="character") LegendPos else{ x=LegendPos[1]; y=LegendPos[2]},
       c(if (AddRegr)"HQ = a ln(RP) + b",                                   #######################
       "Gumbelverteilung", "Gammaverteilung", "Normalverteilung"),
       col=c(if (AddRegr) 5, 4,3,2), lwd=lwd, cex=cex.leg, ...)
# Einfuegen der Parameter
if (PlotParam) {
pxpos1 <- mean(xlim) + ParamPosX/10*(xlim[2]-mean(xlim)) # Parameter-X-Position
pxpos2 <- pxpos1 + diff(xlim)*.03
pypos  <- mean(ylim)+ParamPosY*(ylim[2]-mean(ylim))/10
legend(pxpos1,pypos, paste(parameter[,1]),   adj=1, bty="n", cex=cex.par)
legend(pxpos2,pypos, round(parameter[,2],3), adj=0, bty="n", cex=cex.par) }
} # end if plot
# Endausgabe
return(list(discharge=discharge, parameter=parameter))
}
# Und hier endet die Funktion.
# Wenn jemand R-Skripte fuer MLH-Schaetzverfahren hat, waere ich sehr dankbar dafuer
# im Paket MASS gibt es fitdistr, ich hatte bisher aber keine Zeit dazu.
# Derzeit sind nur die Normal-, Gamma- und Gumbelverteilung drin, die alle ihre
# Parameter aus den Momenten (mean, sd) der Daten holen.
# GEV und Weibull muessen mindestens noch rein! siehe evd:::fgev

