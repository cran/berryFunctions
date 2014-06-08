# Berry Boessenkool, 2011/12
colPoints <- function(
  x,
  y, # x,y: Vektoren mit den Koordinaten der zu zeichnenden Punkte
  z, # Vektor oder Matrix mit zugehoerigen farbenbedingende "Hoehen"-werte
  Range=range(z, finite=TRUE), # zwei Werte fuer Farb-enden Blau und Rot. man kann in Funktionen als Argument mit einer Defaultzuweisung keine Funktion verwenden, daher ist Range gross geschrieben.
  numcol=100, # Anzahl Farbabstufungen fuer Default Palette
  col=rev(rainbow(numcol, start=0, end=.7, alpha=1)), # Farbleiste. default: 100-stufige Farbpalette von blau bis rot
  legend=TRUE, # verwendet colPointslegend, weiter unten angegeben
  add=TRUE, # assoziativ zu points. Sollen Punkte zum bestehenden (!) Plot hinzugefuegt werden? Bei add=F wird neu gezeichnet.
  xlab=substitute(x),
  ylab=substitute(y), # Achsenbeschriftungen
  las=1, # LabelAxisStyle: alle Labels horizontal (nur bei add=FALSE)
  pch=16, # PointCHaracter, das normale pch, siehe ?par
  ...) # Weitere Graphikargumente, zB cex, xlim (bei add=F), mgp, main, sub, asp (bei add=F), usw. Wichtig: col geht natuerlich nicht.
{                   
# z auf 1 bis Laenge Farbreihe (default 100) normieren und ganzzahlig runden:
mod <- lm(c(1,length(col)) ~ Range)[[1]]    # beim Plotten erfolgt der Zugriff auf
Zr <- round(z*mod[2]+mod[1])                # Farben mittels Indizierung mit zz
# Zwischenabsicherung:
if(diff(range(z, finite=TRUE))==0) stop("all z-values are equal.")
#
# a) Regulaerer Fall: z ist ein Vektor
# handhabung:
if(is.vector(z))
   {# Absicherung:
   if(!(length(x)==length(y) & length(x)==length(z)))
      stop("vectors x,y,z are not all of the same length!")
   xx<-x; yy<-y; zz<-Zr
   } else
# b) z ist eine Matrix: class(z) = matrix, data.frame, array (2D)
# So kann colpoints analog zu image, persp u.ae. mit einer Matrix verwendet werden
   {
   if(missing(x)) x <- 1:ncol(z)
   if(missing(y)) y <- nrow(z):1
   if(!(length(x)==ncol(z) & length(y)==nrow(z)))
     stop("Dimension of z (ncol*nrow) is not length(x) * length(y)!")
   xx <- rep(x, each=length(y)); yy <- rep(y, length(x)); zz <- as.vector(Zr)
   }
#
# bei add=TRUE Punkte zum bestehenden Plot hinzufuegen, sonst neu zeichnen:
if(add) {    points(xx, yy, col=col[zz], pch=pch, ...)
} else plot(xx, yy, col=col[zz], xlab=xlab, ylab=ylab, pch=pch, las=las, ...)
# Legende automatisch hinzufuegen:
if(legend) colPointsLegend(Range=Range, col=col, ...)
}
