\name{logAxis}
\alias{logAxis}
\title{label logarithmic axes}
\description{shortcut to calling \code{\link{logVals}}, \code{\link{axis}} and \code{\link{abline}}}
\usage{logAxis(from=-7, side=1, lcol="grey", lty=1, lwd=1, expr, logargs=NULL, las=1, ...)}
\arguments{
  \item{from}{Lower exponent OR vector with data, as in \code{\link{logVals}}. DEFAULT: -7}
  \item{side}{which \code{\link{axis}} is to be labeled? DEFAULT: 1}
  \item{lcol}{Color of gridlines drawn in the graph with \code{\link{abline}}, NA to suppress. DEFAULT: "grey"}
  \item{lty, lwd}{Type of gridlines. DEFAULT: 1}
  \item{expr}{Expression drawing over the ablines, like (points(x,y). Can be code within {braces}.}
  \item{logargs}{List of arguments passed to \code{\link{logVals}}. DEFAULT: NULL}
  \item{las}{LabelAxisStyle for the orientation of the labels. DEFAULT: 1}
  \item{\dots}{further arguments passed to axis, like \code{lwd, col.ticks, hadj, lty}, ...}
}
\value{An invisible list with
  \item{vals}{Values for lines and label positions}
  \item{labs}{Formatted values for labels}
  \item{all}{Values for lines}
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Sept 2014}
\seealso{\code{\link{logVals}}, \code{\link{log10}}}
\examples{
x <- 10^runif(200, -1, 2)
plot(x, 1:200, xaxt="n", log="x")
logAxis(x)
# overplot vertical lines:
logAxis(x, expr=points(x, 1:200), logargs=list(base=c(1,2,5)), col=2 )
}
\keyword{aplot}
\keyword{dplot}
