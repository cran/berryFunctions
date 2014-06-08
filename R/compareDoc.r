# compare Documentation file section arguments with .r-source
# Berry Boessenkool, June 2014
# This assumes the following structure of source code:
# MyFun <- function(
# arg1, # Explanation of this item
# arg2=TRUE, # Ditto, with default
# arg3)


# ToDo: have argument defaults and usage checked
# deparse(substitute(fun))

compareDoc <- function(
  fun, # Character string. Function (== filename) with correct structure in source code.
  path="S:/Dropbox/Public/BerryFunctions" # Path to package in development containing folders "R" and 'man'.
  )
{
owd <- setwd(path)
# .r
getargs <- function()
   {source(paste0("r/",fun,".r"), local=TRUE)
   names(formals(get(fun))) }
# .Rd
rdfile <- scan(file=paste0("man/",fun,".Rd"), what="char", sep="\n", quiet=TRUE)
anf <- grep("\\arguments{", rdfile, fixed=TRUE) +1
end <- tail( grep("\\item{", rdfile, fixed=TRUE) , 1)
if (end < anf) stop("Argument section was not correctly identified!")
#
arg <- sapply( strsplit(rdfile[anf:end], "item{", fixed=TRUE), "[", 2)
arg <- sapply( strsplit(arg, "}", fixed=TRUE), "[", 1)
arg <- gsub("\\dots", "...", arg, fixed=TRUE)
#
missing_in_Rd <- getargs()[ ! getargs() %in% arg ]
missing_in_R <- arg[ ! arg %in% getargs() ]
if( length(missing_in_Rd) > 0 ) cat(fun, ": ", paste(missing_in_Rd, collapse=" & "), " is missing in Documentation (.Rd-file).\n")
if( length(missing_in_R) > 0 )  cat(fun, ": ", paste(missing_in_R , collapse=" & "), " is in Documentation (.Rd-file) but not in source code (.r-file).\n")
#
} # End of Function

