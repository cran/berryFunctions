# Function to label logarithmic axes
# berry-b@gmx.de, Feb 2014, idea 2013

logAxis <- function(
  from=-7,     # Lower exponent OR vector with data, as in \code{\link{logVals}}
  side=1,      # which \code{\link{axis}} is to be labeled?
  lcol="grey", # Color of gridlines drawn in the graph with \code{\link{abline}}, NA to suppress.
  lty=1, lwd=1,# Type of gridlines
  expr,        # Expression drawing over the ablines, like (points(x,y). Can be code within {braces}.
  logargs=NULL,# List of arguments passed to \code{\link{logVals}}
  las=1,       # LabelAxisStyle for the orientation of the labels
  ...)         # further arguments passed to axis, like \code{lwd, col.ticks, hadj, lty}, ...
{
lv <- do.call(logVals, owa(list(from=from), logargs) )
if(side==1 | side==3)
abline(v=lv$all, col=lcol, lty=lty, lwd=lwd)
else
abline(h=lv$all, col=lcol, lty=lty, lwd=lwd)
box("plot")
axis(side=side, at=lv$vals, labels=lv$labs, las=las, ...)
if(!missing(expr)) expr
return(invisible(lv))
}

if(FALSE){
}
