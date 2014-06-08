colPointsLegend <- function(
   xpos=0.5,       # position of topleft corner of legend. relative to plot region (0:1)
   ypos=1,
   length=0.5,     # size of color bar, relative, 0 to 1
   width=0.05,
   z,              # Values used for the default of Range. Matrix oder Vektor
   Range=range(z, finite=TRUE), # 2 values for the ends of the color spectrum
   numcol=100,     # if default palette is used: number of color shades
   alpha=1,        # Transparency for default palette. 0 is transp, 0.5 semitransp.
   col=rev(rainbow(numcol, start=0, end=.7, alpha=alpha)), # colors that are used
   at=Pretty,      # values at which labels are positioned. Default uses internal calculation
   atminmax=FALSE, # Should the extrema of the legend be added to "at"?
   labels=at,      # labels that are written at the position of "at".
   xpd=TRUE,       # Should the legend expand outside of the plot region into the margins?
   lines=TRUE,     # plot black lines in the color bar at "at"?
   horizontal=TRUE,# horizontal bar? else vertical bar is drawn, with length and width exchanged
   labelpos=1,     # position of labels relative to the bar: below, left, above, right
   white=NA,       # coordinates for white polygon drawn underneath the legend
   ...)            # further arguments passed to text, e.g. cex, font, col. But NOT adj!
{
# Labelzahlen auswaehlen (nicht groesser oder kleiner als die Farbskala):
if(xpd) op <- par(xpd=TRUE)
p <- pretty(Range); Pretty <- if(Range[1]<Range[2])
                              { p[p>=Range[1] & p<=Range[2]]
                              } else  p[p<=Range[1] & p>=Range[2]] 
if(atminmax) Pretty <- c(Range[1], Pretty, Range[2])
rm(p)
# Aktuelle Graphikgrenzen fuer die Legendenposition ermitteln:
u <- par()$usr
# Legendenposition in Absolutwerten:
xpos <- u[1] + xpos * (u[2]-u[1])
ypos <- u[3] + ypos * (u[4]-u[3])
#                                       
# Falls weiss in Polygonkoordinaten des Ursprungsplots (absolutwerte)  gegeben, 
# weisse Flaeche unter Legende und Beschriftung setzen.
polygon(white, col="white", border=NA)
# ToDo: make it relative and automatic (dependent on cex, legend position, horizontal, labelpos) 
# dazu cex=1 als Argument hinzufuegen und white=F als default, handling auf T/F bezogen
#
# a) fuer waagerechte Legende: --------------------------------------------------
if(horizontal)
{
# Legendenausdehnung in Absolutwerten:  
      length <- length*(u[2]-u[1])
      width <- width*(u[4]-u[3])
# Einzelne Positionen (fuer jedes einzelene der length(col) Polygone / Farbfelder)
      n_col <- length(col) 
      Eposx <- xpos + 0:n_col*length/n_col
# Farbreihe zeichnen, bei falschem labelpos Abbruch:
      if(labelpos %in% c(1,3,5)) {
      for(i in 1:n_col) { polygon(  c(Eposx[i], rep(Eposx[i+1],2), Eposx[i]) ,
            rep(c(ypos, ypos-width), each=2)  , border=NA, col=col[i])  }
      } else stop("Wrong labelpos. In a horizontal legend, only 1 (below legend bar), 3 (above), and 5(on top of bar) are possible.")
# Labels vorbereiten (Positionen ermitteln):
      mod <- lm( c(xpos, xpos+length) ~ Range )[[1]]
      Epostext <- at*mod[2] + mod[1]
# ggf. Linien zeichnen:
      if(lines) segments(x0=Epostext, y0=ypos, y1=ypos-width)
# Label vorbereiten:
      if(labelpos==1) { YposT <- ypos-1.1*width;  adjT <- 1
      } else if(labelpos==3) { YposT <- ypos+0.1*width; adjT <- 0
      } else if(labelpos==5){ YposT <- ypos-0.5*width; adjT <- 0.5 }
# Label schreiben:
      text(Epostext, YposT, labels, adj=c(0.5, adjT), ...)
#
# b) fuer senkrechte Legende (key): --------------------------------------------- 
} else {
# Legendenausdehnung in Absolutwerten:  
      length <- length*(u[4]-u[3])
      width <- width*(u[2]-u[1])
# Einzelne Positionen (fuer jedes einzelene der length(col) Polygone / Farbfelder)
      n_col <- length(col) 
      Eposy <- ypos - length + 0:n_col*length/n_col
# Farbreihe zeichnen, bei falschem labelpos Abbruch:
      if(labelpos %in% c(2,4,5)) {
      for(i in 1:n_col) { polygon(  rep(c(xpos, xpos+width), each=2),
             c(Eposy[i], rep(Eposy[i+1],2), Eposy[i]), border=NA, col=col[i])  }
      }else stop("Wrong labelpos. In a vertical legend, only 2 (left of legend bar), 4 (right of), and 5(on top of) are possible..") 
# Labels vorbereiten (Positionen ermitteln):
      mod <- lm( c(ypos-length, ypos) ~ Range )[[1]]
      Epostext <- at*mod[2] + mod[1]
# ggf. Linien zeichnen:
      if(lines) segments(x0=xpos, y0=Epostext, x1=xpos+width)
# Label vorbereiten:
      if(labelpos==2) { XposT <- xpos-0.1*width;  adjT <- 1
      }else if(labelpos==5) { XposT <- xpos+0.5*width; adjT <- 0.5
      }else if(labelpos==4){ XposT <- xpos+1.1*width; adjT <- 0 }
# Label schreiben:
      text(XposT, Epostext, labels, adj=c(adjT, 0.5), ...)
} 
if(xpd) par(op) else box() # Fuer den Fall, dass Legenden den Rand ueberplotten:
}
# ToDo: add inset argument as in legend
# maybe add "title" Argument with good placement default
# get coordinates for polygon from text size as in textField
# vielleicht: Argument horizontal=T oder F ersetzen mit dir = "h" oder "v"
# Change colPoints so that it do.call colPointsLegend with a list of arguments
#
