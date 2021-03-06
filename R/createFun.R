#' create function framework
#' 
#' create a file with a complete (Roxygen) framework for a new function in a package
#' 
#' @details Tries to open the file in the standard editor for .R files using \code{\link{system2}}
#' 
#' @return file name as character string
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, March 2016
#' @seealso \code{\link{system2}}, \code{\link{funSource}}, Roxygen2:
#'          \url{https://cran.r-project.org/package=roxygen2/vignettes/rd.html}
#' @keywords documentation
#' @export
#' @examples

#' #createFun("myNewFunction")

#' @param fun  Character string or unquoted name. Function that will be created with identical filename.
#' @param path Path to package in development (including package name itself).
#'             Is passed to \code{\link{packagePath}}. DEFAULT: "."
#' @param open Logical: open the file? If several instances of Rstudio are open,
#'             the last one (not necessarily the active one) will be used.
#'             DEFAULT: TRUE
#' 
createFun <- function(
fun,
path=".",
open=TRUE
)
{
# check and deparse input:
fun <- deparse(substitute(fun))
fun <- gsub("\"", "", fun, fixed=TRUE)
if(length(fun) >1)     stop("'fun' must be a single function name.")
if(length(path)>1)     stop("'path' must be a single character string.")
# Filename
path <- packagePath(path)
rfile <- paste0(path,"/R/",fun,".R")
rfile <- newFilename(rfile) # append _1 if existent
#
# Date
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
date <- paste0(format(Sys.Date(), "%b %Y"), "\n")
Sys.setlocale("LC_TIME", lct)
#
# Write function structure
part1 <- paste0(
"' @title title
' @description description
' @details detailsMayBeRemoved
' @aliases aliasMayBeRemoved
' @section Warning: warningMayBeRemoved
' @return ReturnValue
' @author Berry Boessenkool, \\email{berry-b@@gmx.de}, ", date,
"' @seealso \\code{\\link{help}}, \\code{graphics::\\link[graphics]{plot}}
' @keywords aplot
 @importFrom package fun1 fun2
' @export
' @examples
' ",fun,"(rnorm(20))
'
' @param a     Numerical vector.
' @param plot  Logical. Should values be plotted? This can be turned off if
'              only the computation results are needed. DEFAULT: TRUE
' @param dummy currently_Unused
' @param \\dots Further arguments passed to \\code{\\link{plot}}
'
")
part2 <- paste0("\n",
fun," <- function(
a,
plot=TRUE,
dummy,
...
)
{
if(plot) plot(a, ...)
# Output:
return(invisible(a))
}
")

part1 <- paste0("#", strsplit(part1, "\n", fixed=TRUE)[[1]])
part1 <- paste(part1, collapse="\n")
cat(part1,part2, file=rfile, sep="")
# Open the file with the program associated with its file extension:
if(open) openFile(rfile)
# return file name:
invisible(rfile)
}

