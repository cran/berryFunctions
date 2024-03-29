#' check file existence
#' 
#' check whether files exist and give a useful error/warning/message
#' 
#' @return TRUE/FALSE, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2016
#' @seealso \code{\link{file.exists}}
#' @keywords file
#' @export
#' @examples
#' is.error( checkFile("FileThatDoesntExist.txt")  )
#' checkFile("FileThatDoesntExist.txt", warnonly=TRUE)
#' checkFile("FileThatDoesntExist.txt", warnonly=TRUE, trace=FALSE)
#' 
#' checkFile("./", warnonly=TRUE)
#' checkFile(c("./",".//"), warnonly=TRUE)
#' 
#' \dontrun{## Excluded from CRAN checks because of file creation
#' # Vectorized:
#' file.create("DummyFile2.txt")
#' checkFile("DummyFile2.txt/")
#' checkFile(paste0("DummyFile",1:3,".txt"), warnonly=TRUE)
#' is.error(checkFile(paste0("DummyFile",1:3,".txt") ), TRUE, TRUE)
#' file.remove("DummyFile2.txt")
#' 
#' is.error(compareFiles("dummy.nonexist", "dummy2.nonexist"), TRUE, TRUE)
#' is.error(checkFile("dummy.nonexist"), TRUE, TRUE)
#' }
#' 
#' dingo <- function(k="brute.nonexist", trace=TRUE)
#'          checkFile(k, warnonly=TRUE, trace=trace)
#' dingo()
#' dingo("dummy.nonexist")
#' 
#' upper <- function(h, ...) dingo(c(h, "dumbo.nonexist"), ...)
#' upper("dumbo2.nonexist")
#' upper(paste0("dumbo",2:8,".nonexist"))
#' upper(paste0("dumbo",2:8,".nonexist"), trace=FALSE)
#' 
#' 
#' @param file     Filename(s) as character string to be checked for existence.
#' @param warnonly Logical: Only issue a \code{\link{warning}} instead of an
#'                 error with \code{\link{stop}}? DEFAULT: FALSE
#' @param trace    Logical: Add function call stack to the message? DEFAULT: TRUE
#' @param pwd      Logical: Print working directory in message? DEFAULT: TRUE
#' @param nprint   Integer: number of filenames to be printed. 
#'                 The rest is abbreviated with (and n others). DEFAULT: 2
#' 
checkFile <- function(
file,
warnonly=FALSE,
trace=TRUE,
pwd=TRUE,
nprint=2
)
{
# check actual file existence:
exi <- file.exists(file)
# warn or stop if file nonexistent:
if(any(!exi))
  {
  # existing without trailing slashes?
  # remove trailing slashes for windows folders:
  file_ns <- sub("[/|\\]+$", "", file)
  exi_ns <- file.exists(file_ns)
  exi_ns <- exi_ns & !exi
  # tracing the calling function(s):
  Text1 <- if(trace) traceCall(prefix="in ", suffix=": ") else ""
  Text2 <- if(pwd) paste0("(With current getwd: ", getwd(), ")\n") else ""
  # prepare message:
  Text3 <- if(sum(!exi)>1) paste0("The following ",sum(!exi)," files don't exist: ") else 
                                  "The following file doesn't exist: "
  Text4 <- if(sum(!exi)>nprint) paste0(toString(file[!exi][1:nprint]), " (and ",sum(!exi)-nprint," others)") else toString(file[!exi])
  Text5 <- if(sum(exi_ns)==1) " (But exists without trailing slashes)." else 
           if(sum(exi_ns) >1) paste0("\n(But ",sum(exi_ns)," exist without trailing slashes).") else ""
  Text <- paste0(Text1,Text2,Text3,Text4,Text5)
  if(warnonly) warning(Text, call.=!trace) else stop(Text, call.=!trace)
  }
return(invisible(exi))
}
