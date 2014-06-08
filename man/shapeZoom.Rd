\name{shapeZoom}
\alias{shapeZoom}
\title{zoom into ESRI shapefiles}
\description{uses locator to define zooming area}
\usage{shapeZoom(shp, Time = 3, steps = 3, ...)}
\arguments{
  \item{shp}{Shapefile Object.}
  \item{Time}{Duration of zooming (speed) in seconds.. DEFAULT:5 }
  \item{steps}{number of single zoomlevels. DEFAULT:2 }
  \item{\dots}{Further arguments passed to plot.}
}
\value{None - only plots}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, ca 2012}
\seealso{\code{\link{pointZoom}} for simple scatterplots. \code{\link{changeAttribute}} and \code{\link{showAttribute}} for more Shapefile-related operations}
\examples{
\dontrun{
library(maptools) # for readShapeSpatial and dataset
xx <- readShapeLines(system.file("shapes/fylk-val.shp", package="maptools")[1],
                     proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))

windows(record=TRUE)
plot(xx)
shapeZoom(xx)
# With pgUp and Dn, you can zoom out again

plot(xx)
shapeZoom(xx, steps=8)



}
}
\keyword{aplot}
\keyword{iplot}
\keyword{dynamic}

