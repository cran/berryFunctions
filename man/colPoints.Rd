\name{colPoints}
\alias{colPoints}
\title{add points to plot with color relative to third dimension data}
\description{
Draw colored points for 3D-data in a 2D-plane. Color is relative to third dimension, currently only on a linear scale.
Can take 3 vectors or, as in \code{\link{image}}, 2 vectors and a matrix for z.}
\usage{colPoints(x, y, z, Range=range(z, finite=TRUE), numcol=100,
col=rev(rainbow(numcol, start=0, end=0.7, alpha=1)), legend=TRUE, add=TRUE,
xlab=substitute(x), ylab=substitute(y), las=1, pch=16, ...)}
\arguments{
  \item{x}{x coordinates. Vector}
  \item{y}{y coordinates. Vector}
  \item{z}{z values beloning to coordinates. Vector or matrix}
  \item{Range}{Range for the color. DEFAULT: \code{\link{range}}(z). Can manually only be set to larger range than range(z) currently! (It wouldn't know how to color values that are below the lowest value in Range)}
  \item{numcol}{Number of color nuances in the default color palette. DEFAULT: 100}
  \item{col}{Vector of colors to be used. Not tested outside default yet! DEFAULT: \code{\link{rainbow}} from blue (lowest) to red (highest value in Range)}
  \item{legend}{Logical. Should legend be added? Calls \code{\link{colPointsLegend}}. DEFAULT: TRUE}
  \item{add}{Logical. Should the points be added to current (existing!) plot? If FALSE, a new plot is started. DEFAULT: TRUE (It's called col_points_, after all)}
  \item{xlab}{x-axis label. DEFAULT: substitute as in plot}
  \item{ylab}{y-axis label. DEFAULT: ditto}
  \item{las}{Label Axis Style. Only used when add=FALSE. See \code{\link{par}}. DEFAULT 1 (all labels horizontal)}
  \item{pch}{Point CHaracter. See \code{\link{par}}. DEFAULT 16}
  \item{\dots}{Further graphical arguments, eg cex, xlim (when add=F), mgp, main, sub, asp (when add=F), etc. Note: col does not work, as it is already used}
}
\details{When legend=TRUE, it is not suggested to set arguments to colPointsLegend here via \code{\dots}. Rather use \code{\link{colPointsLegend}} separately.}
\value{no Output. Just drawing.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2011/2012. I'd be thankful to receive a message telling me what you used the function for. I originally used it for plotting depth data in a lake bathymetry...}
\seealso{\code{\link{colPointsLegend}} }
\examples{
plot(1:100, col=rev( rainbow(100, start=0, end=.7)  ), pch=16)

# examples without legend ------------------------------------------------------

# highlight local character of points on a regular grid normally drawn with image:
# library(datasets), normally already loaded in newer R versions.
z <- t(volcano)  ;  x <- 1:ncol(z)  ;  y <- 1:nrow(z)
z[1:7, 1:7]; dim(z)   # example for class(z) = matrix
colPoints(x,y,z, add=FALSE, legend=FALSE)  # takes matrix for z
contour(x,y,t(z), add=TRUE)

image(x,y,t(z), col=rev(rainbow(100, start=0, end=.7)))
# image only takes a regular matrix, but not scatterpoints...

# add single newly measured points to image (fictional data):
i <- c( 22,  40,  80,  45,  60,  63,  30,  70)
j <- c(  5,  33,  12,  56,  20,  40,  45,  45)
k <- c(135, 155, 120, 105, 140, 130, 190, 110) 

colPoints(i,j,k, cex=5, pch="*", Range=c(94, 195), legend=FALSE); points(i,j, cex=4)
text(i, j, k, adj=-0.5, font=2)


# examples with legend ---------------------------------------------------------

# The real purpose of colPoints:
i <- c(22, 40, 80, 45, 60, 63, 30, 70) ;  j <- c(5, 33, 12, 56, 20, 40, 45, 45)
k <- c(135, 155, 120, 105, 140, 130, 190, 110) 

colPoints(i,j,k, cex=2, pch="+", add=FALSE, legend=FALSE)
colPointsLegend(z=k) 
text(65, 52, "Elevation [m above NN.]") # Title argument may be added if I have 
# good ideas on default and possible locations     and     spare time

colPointsLegend(xpos=.2, ypos=.5, length=.75, width=.1, z=k, labelpos=5, atminmax=TRUE)

# Zahlen means numbers, Laenge means length, Breite means width
colPointsLegend(xpos=.5, ypos=.28, length=.4, z=k, Range=c(100, 200), numcol=10, font=3)
text(65,12, "Example with 10 colors.\nnot the same scale as the points!")
colPointsLegend(xpos=.1, ypos=.05, width=.03, z=k, labelpos=3, lines=FALSE)

# colPointsLegend(.01, .9, z=k, horizontal=FALSE)
# not possible, because labelpos default is 1 (below bar)...
colPointsLegend(.01, .9, z=k, horizontal=FALSE, labelpos=4, cex=1.2)
colPointsLegend(.23, .95, z=k, horizontal=FALSE, length=.3, labelpos=5, cex=0.8,
                at=c(130, 150, 170), labels=c(" y", "rr","Be"), lines=FALSE)

args(colPointsLegend)

z <- volcano  ;  x <- 1:ncol(z)  ;  y <- nrow(z):1
colPoints(x,y,z, add=FALSE, legend=FALSE)
# image and contour by default transpose the matrix!
# This is what's really in the data!
colPointsLegend(.3,.05, .6, z=k, labelpos=3) 
# hard to read. White (weiss) background, currently only in absolute (!) coordinates:
colPointsLegend(.3,.05, .6, z=k, white=list(x=c(23,84,84,23), y=c(0,0,6,6)), labelpos=3)
# coordinates of edges, drawn via ?polygon

# Please contact me to tell what error and exception handling is still needed!
# Suggestions for further arguments are welcome.
# A couple are already in the source code (Partly German)
}
\keyword{aplot}
\keyword{hplot}
\keyword{color}
