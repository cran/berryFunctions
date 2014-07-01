\name{showAttribute}
\alias{showAttribute}
\title{Show attribute of ESRI-Shapefile line segment selected by mouseclick}
\description{Click near a line segment of a shapefile, and it's attribute(s) are displayed in the console. 
Currently only works for objects of class SpatialLinesDataFrame.}
\usage{showAttribute(object, object_psp=as.psp(object),
coltoshow=1:ncol(object), ID_col=1, notify=TRUE)}
\arguments{
  \item{object}{object of class SpatialLinesDataFrame. Not tested for other objects yet!}
  \item{object_psp}{convert SpatialLinesDataFrame to line segment pattern. slow conversion, rather define it outside the function, especially when using the function more than once. DEFAULT: as.psp(object)}
  \item{coltoshow}{character vector. column(s) of attribute table to be showed. Should technically work with number(s) as well, not tested yet. DEFAULT: 1:ncol(object)}
  \item{ID_col}{Column to be used for identification. Not fully understood yet... DEFAULT: 1}
  \item{notify}{logical. Are you to be told what to do in the console?. DEFAULT: TRUE}
}
\value{none returned. \code{\link{print}}s attrributes of selected item.}
\section{Background}{
My colleague argues that arcGIS is better than R, because you can click on a line segment and see it's attributes.
I took on the challenge to prove that R can do this just as well...}
\note{I have never used Line-Shapefiles in R before, so I doubt this is highly transferable.
Any Feedback is welcome!\cr
The blue "selection" is overdrawn in \emph{white} when par("bg") == "transparent" (the default)\cr
I have some vague ideas about adding some kind of TCL-TK GU-Interface and allowing to click on another segment, and the info in the interface is updated automatically.
But I don't really know yet how to get that done...
}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Nov. 2012}

\seealso{\code{\link{changeAttribute}}, \code{\link{shapeZoom}}, \code{\link[maptools]{readShapeSpatial}} in the package maptools. }
\examples{

# see ?changeAttribute for more information
# Read Shapefile:

library(maptools)
library(spatstat)
xx <- readShapeLines(system.file("shapes/fylk-val.shp", package="maptools")[1],
                     proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
xx_psp <- as.psp(xx) # slow for bigger shapefiles, so take it out of the function
# if object_psp is not given, it will be calculated each time...

\dontrun{
plot(xx)
showAttribute(xx)
showAttribute(xx, xx_psp)
showAttribute(xx, xx_psp, 3:6)
showAttribute(xx, xx_psp, notify=FALSE) # Finish with "Enter"
}
}
\keyword{spatial}
\keyword{iplot}
