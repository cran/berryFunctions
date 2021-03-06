% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/owa.R
\name{owa}
\alias{owa}
\title{Overwrite argument default lists}
\usage{
owa(d, a, ..., quiet = FALSE)
}
\arguments{
\item{d}{Default arguments (list or vector)}

\item{a}{Arguments specified by user (list or vector).
Can also be a single TRUE, in which case d will be returned.}

\item{\dots}{Names of unchangeable arguments (that will not be overwritten)
as character strings. Can also be a vector with characters strings.}

\item{quiet}{Logical: Should \code{\link{message}} be suppressed if arguments are ignored?
If FALSE (the DEFAULT), this helps users debugging, as they get
notified when arguments they specified were ignored.}
}
\value{
Always a list, disregarding list/vector mode of input
}
\description{
Second ellipsis (three dots) passed to particular functions,
combining default and user-specified argument lists.\cr
\code{owa} can be used in functions that pass argument lists separately to several functions.
Internal defaults can be set per function (eg. one list for \code{\link{plot}}
and one for \code{\link{legend}}). \cr
You can specify which defaults can be overwritten and which should be left unchanged.
See the example section on how to implement this.
}
\examples{
# The motivation behind owa:
testfun <- function(...) {plot(7:11, ...) ; legend("top", "some text", ...)}
testfun()
is.error( testfun(type="o") , tell=TRUE)
# Error: legend doesn't have the argument 'type'!

# How to solve this:
testfun <- function(legargs=NULL, ...) # dots passed to plot
   {
   plot(7:11, ...)
   legend_defaults <- list(x="top", lty=1, col="red", legend="owa rocks!")
   # combine defaults and user specified into final argument list,
   # overwrite arguments ('owa') in the default list unless protected:
   legend_final <- owa(d=legend_defaults, a=legargs, "col", "lwd")
   do.call(legend, args=legend_final)
   }

testfun()
testfun(type="l", col="blue")
testfun(type="o", legargs=list(col="blue", pch=16, lty=3) )
# color in legargs is ignored, as it is defined as unchangeable


#----------------------------------------------------------------------------

# basic tests of owa itself:
d <- list(bb=1:5, lwd="was d", lty=1,   col="gray")
a <- list(bb=3,   lwd=5, lty="from a", wachs="A")
owa(d,a) # all changed, wachs added
owa(d, a, "bb", "lwd") # lty is overwritten, bb and lwd are ignored
owa(d, NULL, "bb", "wachs") # NULL is a good default for argument lists
owa(d, c(HH=2, BBB=3) ) # vectors and lists are all converted to lists
owa(d, list(lwd=5, bb=3, lty="1") ) # order of arguments doesn't matter
owa(d, a, c("bb","lwd") ) # unchangeable can also be a named vector
owa(d, a, c("bb","lwd"), c("lty","dummy") ) # or several vectors


}
\references{
\url{http://stackoverflow.com/questions/3057341}\cr
   \url{http://stackoverflow.com/questions/5890576}\cr
   \url{http://stackoverflow.com/questions/4124900}\cr
   \url{http://stackoverflow.com/questions/16774946}\cr
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Early 2014, Update Oct 2016
}
\keyword{programming}
