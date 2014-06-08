\name{changeAttribute}
\alias{changeAttribute}
\title{Change attribute of ESRI-Shapefile line segment selected by mouseclick}
\description{Click near a line segment of a shapefile, confirm whether it's the correct one, and it's attribute in the given column is changed. 
Currently only works for objects of class SpatialLinesDataFrame.}
\usage{changeAttribute(object, object_psp=as.psp(object), coltochange,
 changeto=NULL, ID_col=1, notify=TRUE)}

\arguments{
  \item{object}{object of class SpatialLinesDataFrame. Not tested for other objects yet!}
  \item{object_psp}{convert SpatialLinesDataFrame to line segment pattern. slow conversion, rather define it outside the function, especially when using the function more than once. DEFAULT: as.psp(object)}
  \item{coltochange}{character. column of attribute table to be changed. Should technically work with a number as well, not tested yet.}
  \item{changeto}{characterstring. If not specified, the user is asked to write it in the console. DEFAULT: NULL}
  \item{ID_col}{Column to be used for identification. Not fully understood yet... DEFAULT: 1}
  \item{notify}{logical. Are you to be told what to do in the console?. DEFAULT: TRUE}
}
\value{The changed ShapefileObject, returned invisibly.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, 25. + 27. Nov. 2012}
\note{I have never used Line-Shapefiles in R before, so I doubt this is highly transferable.
Any Feedback is welcome!\cr
The blue "selection" is overdrawn in \emph{white} when \code{par("bg") == "transparent"} (the default)
}
\section{Background}{
My colleague argues that arcGIS is better than R, because you can click on a street in a forest and change the attribute to bridge. That way, it is no longer an obstacle (barrier) for e.g. frogs changing habitat patches.
I took on the challenge to prove that R can do this just as well...}

\seealso{ \code{\link{showAttribute}}, \code{\link[maptools]{readShapeLines}} in the package maptools. }
\examples{
\dontrun{
# Read and examine Shapefile
# install.packages(maptools) # if not downloaded yet
library(maptools) # for readShapeSpatial  
library(spatstat) # for as.psp.

xx <- readShapeLines(system.file("shapes/fylk-val.shp", package="maptools")[1],
                     proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))

class(xx)  #  "SpatialLinesDataFrame": in addition to the spatial objects,
# there is a data frame of additional variables. Attribute Table viewable with
View(xx)
summary(xx)

xx$KAT <- as.factor(sample(c("l","s","k"), 97, replace=TRUE)) # Category

plot(xx, col=xx$KAT)
axis(1) ; axis(2)

# convert SpatialLinesDataFrame to line segment pattern:
xx_psp <- as.psp(xx)
# takes a while, that's why you want to pass the object to the function,
# so it doesn't need to calculate it every time

xx2 <- changeAttribute(xx, xx_psp, coltochange="KAT", changeto="new")
View(xx2)# One entry should now read III
plot(xx2, col=xx2$KAT) ; axis(1) ; axis(2)

xx3 <- changeAttribute(xx, xx_psp, coltochange="KAT")
# asks the user what the new KAT should be, if changeto is not given
View(xx3)

# annoyed by having to confirm with "y" every time?
xx4 <- changeAttribute(xx, xx_psp, coltochange="KAT", changeto="fr", notify=FALSE)
# the blue line is only shown a second.
# (different from showAttribute, which waits for user to press return.)

# Add new column:
xx5 <- changeAttribute(xx, xx_psp, coltochange="KATblub",
           changeto="new entry", notify=FALSE)
View(xx5) # extra column is added, with warning in case you misspelled.

# Perform this for several line segments:
xx6 <- xx
for(i in 1:3)
xx6 <- changeAttribute(xx6, xx_psp, "KAT", "IV", notify=FALSE)
# As long as no geometry is changed, you can use the "old" psp-Object again.
levels(xx6$KAT)
plot(xx6, col=xx6$KAT) ; axis(1) ; axis(2)

xx7 <- changeAttribute(xx, coltochange="VALINJE_", changeto=115)
summary(xx7["VALINJE_"]) # new maximum

}
}
\keyword{iplot}
\keyword{attribute}
\keyword{manip}
\keyword{spatial}
