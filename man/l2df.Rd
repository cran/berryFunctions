% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l2df.R
\name{l2df}
\alias{l2df}
\title{List to data.frame}
\usage{
l2df(list, byrow = TRUE)
}
\arguments{
\item{list}{List with vectors of irregular length.}

\item{byrow}{Transposed output? DEFAULT: TRUE}
}
\value{
data.frame
}
\description{
Convert list with vectors of unequal length to dataframe, pad with NAs
}
\examples{

eglist <- list(AA=c(6,9,2,6), BB=1:8, CC=c(-3,2) )
eglist
l2df(eglist)  # names are even kept
l2df(eglist, byrow=FALSE)
class(  l2df(eglist, byrow=FALSE)  ) # data.frame (since 2016-05-24)

eglist <- list(AA=c(6,9,2,6), BB="no", CC=c(-3,2) )
eglist
str(l2df(eglist))  # now everything is a character

eg2 <- list(AA=c(6,9,2,6), BB=matrix(1:8, ncol=2), CC=c(-3,2) )
eg2
l2df(eg2, FALSE)
# so a matrix is internally converted to a vector and then used regularly


# Naming ----

eg3 <- list(EE=c(AA=3.4),        FF=c(AA=3.5),        GG=c(AA=3.6))
eg4 <- list(EE=c(AA=3.4,BB=2.4), FF=c(AA=3.5,BB=2.5), GG=c(AA=3.6,BB=2.6))
l2df(eg3)
l2df(eg4)
l2df(eg3, byrow=FALSE)
l2df(eg4, byrow=FALSE)

eg3 <- list(c(AA=3.4),        c(AA=3.5),        c(AA=3.6))
eg4 <- list(c(AA=3.4,BB=2.4), c(AA=3.5,BB=2.5), c(AA=3.6,BB=2.6))
l2df(eg3)
l2df(eg4)
l2df(eg3, byrow=FALSE)
l2df(eg4, byrow=FALSE)

eg3 <- list(EE=c(3.4),     FF=c(3.5),     GG=c(3.6))
eg4 <- list(EE=c(3.4,2.4), FF=c(3.5,2.5), GG=c(3.6,2.6))
l2df(eg3)
l2df(eg4)
l2df(eg3, byrow=FALSE)
l2df(eg4, byrow=FALSE)

eg3 <- list(EE=c(3.4),     c(3.5),     c(3.6))
eg4 <- list(EE=c(3.4,2.4), c(3.5,2.5), c(3.6,2.6))
l2df(eg3)
l2df(eg4)
l2df(eg3, byrow=FALSE)
l2df(eg4, byrow=FALSE)


# Lists with dfs ----

eg5 <- list(AA=c(6,9,2,6), BB=data.frame(CC=1:8, DD=4:-3), EE=c(-3,2) )
eg5
is.error( l2df(eg5), tell=TRUE )# it is not possible to do this with a data.frame

# If you have a list with only data.frames, you could use the following:
eg6 <- list(AA=data.frame(BB=1:8, CC=4:-3), DD=data.frame(EE=23:24, FF=c(-3,2)))
eg6
do.call(cbind, eg6) # but this recycles the values of shorter tables!
colnames(eg6$DD) <- colnames(eg6$AA)
do.call(rbind, eg6)
# check some of the links above for more solutions...

}
\references{
\url{https://stackoverflow.com/questions/5531471/combining-unequal-columns-in-r}\cr
  \url{https://stackoverflow.com/questions/15753091/convert-mixed-length-named-list-to-data-frame}\cr
  \url{https://stackoverflow.com/questions/5942760/most-efficient-list-to-data-frame-method}\cr
  \url{https://stackoverflow.com/questions/8799990/converting-given-list-into-dataframe}\cr
  \url{https://stackoverflow.com/questions/4227223/r-list-to-data-frame}
}
\seealso{
\code{\link{l2array}}, \code{\link{sapply}}, \code{\link{sortDF}}.
         If you have a LARGE list each with the same number of values,
         use the (much!) faster: \code{\link{simplify2array}} or \code{plyr::quickdf}.
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Jan 2014
}
\keyword{list}
\keyword{manip}
