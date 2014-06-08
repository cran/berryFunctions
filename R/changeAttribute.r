changeAttribute <- function(
   object,   # object of class SpatialLinesDataFrame. Not tested for other objects yet!
   object_psp=as.psp(object), # convert SpatialLinesDataFrame to line segment pattern. slow conversion, rather define it outside the function, especially when using the function more than once
   coltochange, # column of Attribute table to be changed (character)
   changeto=NULL, # characterstring. If not specified, user is asked to write it in the console
   ID_col=1, # Column to be used for identification. Not fully understood yet...
   notify=TRUE) # logical. Are you to be told what to do in the console?
{
objname <- deparse(substitute(object))
require(spatstat)# for as.psp, ppp, nearestsegment 
# Some basic "idiot-proofing":
if(class(object)!="SpatialLinesDataFrame") warning(paste("Not yet tested for objects",
     "that are not of class SpatialLinesDataFrame created with readShapeSpatial in maptools!"))
if(missing(coltochange)) stop("You have to specify the name of the column to be changed")
# Write instruction into the console:
if(notify){
   cat("please click near the line segment whose attribute you wish to change\n")
   flush.console() }
# Have the user identify the line
k <- locator(1)
# convert point(s) to ppp-object
kp <- ppp(k$x, k$y, xrange=par()$usr[1:2], yrange=par()$usr[3:4] )
# Find nearest segment
ns <- nearestsegment(kp, object_psp)
# Find ID of segment of original object
m <- object_psp$marks[ns]
# Highlight selected segment
lines(object[object@data[,ID_col]==m,], col="cyan", lwd=5)
# ask the user if the function's going to change the right line segment:
if(notify){
if(is.null(changeto))
   {changeto <- readline(paste("Thanks.\nIn case you selected the wrong segment,",
   "type 'getmeout' to exit and select a different segment.\n",
   "What do you want to change this line element's attribute to? : "))
   correct <- ifelse(changeto=="getmeout", "n", "y")
   } else
   {correct <- readline(paste0("Is this the line element ",
   "whose attribute you want to change to '", changeto, "'?\n",
   "please answer with 'y' or 'n', confirm with RETURN. y/n : "))
   }
   # idiot proof input:
   if( ! correct %in% c("y", "n")) {cat("please answer with 'y' or 'n', confirm with RETURN\n")
   correct <- readline("Is this the line to be changed?  y/n : ")
   }
} else {# when notify = FALSE, the user is not asked:
   Sys.sleep(1) # let the blue line show a second
   correct <- "y"
   if(is.null(changeto))  changeto <- readline("What do you want to change this line element's attribute to? : ")
   }
#
if(correct=="y")
  # prepare the actual work:
  {# Expand levels when the column in the attribute table is a factor
  if(class(object[[coltochange]]) =="factor")
  levels(object[[coltochange]]) <- c(levels(object[[coltochange]]), changeto)
  #
  if(!coltochange %in% names(object)) warning(paste0("Column '", coltochange,
                     "' was not in the attribute table, so it's added now."))
  # Do the actual actual work:
  object[[coltochange]][object@data[,ID_col]==m] <- changeto
  # Erase the light blue lines
  lines(object[object@data[,ID_col]==m,], col=ifelse(par("bg")=="transparent", "white", par("bg")), lwd=5)
  lines(object[object@data[,ID_col]==m,])
  if(notify) cat("The changed shapefile is returned invisibly, don't forget to assign it.\n")
  return(invisible(object))
  }
if(correct=="n") { cat("nothing is changed in object '", objname, "'\n", sep="")
  lines(object[object@data[,ID_col]==m,], col=ifelse(par("bg")=="transparent", "white", par("bg")), lwd=5)
  lines(object[object@data[,ID_col]==m,])}
} # End of function
