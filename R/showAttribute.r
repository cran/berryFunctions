# Show info about selected line Segment

showAttribute <- function(
  object,   # object of class SpatialLinesDataFrame. Not tested for other objects yet!
  object_psp=as.psp(object), # convert SpatialLinesDataFrame to line segment pattern. slow conversion, rather define it outside the function
  coltoshow=1:ncol(object), # column of Attribute table to be showed (number or character)
  ID_col=1, # Column to be used for identification. Not fully understood yet...
  notify=TRUE) # logical. Are you to be told what to do in the console?

{
require(spatstat)
# Some basic "idiot-proofing":
if(class(object)!="SpatialLinesDataFrame") warning(paste("Not yet tested for objects",
   "that are not of class SpatialLinesDataFrame created with readShapeSpatial in maptools!"))
# Write instruction into the console:
if(notify)
  {
  cat("please click near the line segment whose attribute you wish to show\n")
  flush.console()
  }
# Have the user identify the line
k <- locator(1)
# convert point(s) to ppp-object
kp <- ppp(k$x, k$y, xrange=par()$usr[1:2], yrange=par()$usr[3:4] )
# Find nearest segment
ns <- nearestsegment(kp, object_psp)
# Find GIS_ID of segment of original object
m <- object_psp$marks[ns]
# Highlight selected segment
lines(object[object@data[,ID_col]==m,], col="cyan", lwd=5)
# more fool-proofing:
#if(!coltoshow %in% names(object)) stop(paste("Column", coltoshow, "is not in the attribute table!"))
# Do the actual work:
print(t(object[object@data[,ID_col]==m, coltoshow]@data))
# Erase the light blue lines
if(notify)
  {
  readline("press RETURN to turn off higlighting and end the function. ")
  flush.console()
  } else   readline()
lines(object[object@data[,ID_col]==m,], col=ifelse(par("bg")=="transparent", "white", par("bg")), lwd=5)
lines(object[object@data[,ID_col]==m,])
} # End of function
