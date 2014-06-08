\name{colPointsLegend}
\alias{colPointsLegend}
\title{legend for colPoints}
\description{Adds legends to plots created or enhanced with \code{\link{colPoints}}}
\usage{colPointsLegend(xpos=0.5, ypos=1, length=0.5, width=0.05, z,
Range=range(z, finite=TRUE), numcol=100, alpha=1,
col=rev(rainbow(numcol, start=0, end=0.7, alpha=alpha)),
at=Pretty, atminmax=FALSE, labels=at, xpd=TRUE, lines=TRUE, horizontal=TRUE,
labelpos=1, white=NA, ...)}
\arguments{
  \item{xpos}{Position of left end of the legend-color-bar; Relative to plot region, between 0 and 1. DEFAULT: 0.5 (middle of plot)}
  \item{ypos}{Upper position. If(xpd), you could use 1.1 to set legend into the margin. DEFAULT: 1 (top)}
  \item{length}{Length of legend bar, relative between 0 and 1. DEFAULT: 0.5 (half the plot)}
  \item{width}{Width of legend, also relative. DEFAULT: 0.05}
  \item{z}{Values used for the default of Range. Matrix oder Vektor}
  \item{Range}{2 values for the ends of the color spectrum. DEFAULT: range(z)}
  \item{numcol}{Number of color nuances in the default color palette. DEFAULT: 100}
  \item{alpha}{Transparency for default palette. 0 is transp, 0.5 semitransp. DEFAULT: 1 (intransparent)}
  \item{col}{Colors to be used. DEFAULT: from blue to red}
  \item{at}{values at which labels are positioned. DEFAULT: Pretty, calculated inside the function, an altered result from \code{\link{pretty}}.}
  \item{atminmax}{Logical. Should the extreme ends of Range be labeled additionally? DEFAULT: FALSE}
  \item{labels}{The labels that are written at the position of "at". DEFAULT: at}
  \item{xpd}{Should the legend expand outside of the plot region into the margins? DEFAULT: TRUE}
  \item{lines}{Logical. Should black lines over the color bar mark the exact position of "at"? DEFAULT: TRUE}
  \item{horizontal}{Logical. Should the legend be drawn horizontally? If FALSE, a vertical legend is drawn, with length and width exchanged. DEFAULT: TRUE}
  \item{labelpos}{Position of labels relative to the bar. Possible: 1 (below), 2 (left), 3 (above), 4 (right), 5(on top of bar). DEFAULT: 1}
  \item{white}{Coordinates for white \code{\link{polygon}} drawn underneath legend, making labels readable. DEFAULT: NA}
  \item{\dots}{Further arguments passed to \code{\link{text}}, e.g. cex, font, col. But NOT adj! }
}
\value{none, adds legend bar to current plot}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 2012}
\seealso{\code{\link{colPoints}} for examples!}
\examples{
# See colpoints
}
\keyword{aplot}
\keyword{color}
