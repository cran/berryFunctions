\name{compareDoc}
\alias{compareDoc}
\title{compare Documentation file with source file}
\description{Compare arguments in Documentation file (.Rd) with those in the source file (.r). Saves time when \code{build}ing package.}
\usage{compareDoc(fun, path = "S:/Dropbox/Public/BerryFunctions") }
\arguments{
  \item{fun}{Character string. Function (== filename) with correct structure in source code.}
  \item{path}{Path to package in development containing folders 'R' and 'man'. DEFAULT: "S:/Dropbox/Public/BerryFunctions"}
}
\details{This assumes the following structure of source code:\cr
          \code{MyFun <- function(}\cr
          \code{arg1, # Explanation of this item}\cr
          \code{arg2=TRUE, # Ditto, with default}\cr
          \code{arg3)}
}
\value{None. Cats into the console if there are inconsistancies.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, June 2014}
\section{Warning}{This is highly specific to my way of working, don't rely blindly on it.}
\seealso{\code{\link{createDoc}}, \code{\link{scan}}, \code{\link{cat}}}
\examples{
\dontrun{
FNS <- sapply( strsplit( dir("../berryFunctions/R/"), ".", fixed=TRUE ) , "[", 1)
for(i in FNS) compareDoc(i)
}
}
\keyword{documentation}
