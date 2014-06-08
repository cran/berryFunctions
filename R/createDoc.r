# Create Documentation file section arguments from .r-source
# Berry Boessenkool, June 2014
# This assumes the following structure of source code:
# MyFun <- function(
# arg1, # Explanation of this item
# arg2=TRUE, # Ditto, with default
# arg3)

# PS: I know of Roxygen, but it doesn't really get this structure, as far as I know.

createDoc <- function(
  fun, # Character string. Function (== filename) with correct structure in source code.
  path="S:/Dropbox/Public/BerryFunctions" # Path to package in development containing folders "R" and 'man'.
  )
{
owd <- setwd(path)
rdfile <- paste0("man/",fun,".Rd")
#
rfile <- scan(file=paste0("r/",fun,".r"), what="char", sep="\n", quiet=TRUE)
anf <- grep("<- function", rfile)[1]
end <- grep("{", rfile, fixed=TRUE)[1]    #}
if (end < anf) stop("Argument section was not correctly identified!")
# HEADER
cat(paste0("\\name{", fun, "}\n\\alias{", fun, "}\n"), file=rdfile)
cat(paste0("\\title{}\n\\description{}\n\\usage{", fun ), file=rdfile, append=TRUE)
# USAGE
source(paste0("r/",fun,".r"))
sink("TempstufftodeleteBerryRocks.txt")
str(args(get(fun)))
sink()
usagecontent <- paste(readLines("TempstufftodeleteBerryRocks.txt"), collapse="" )
unlink("TempstufftodeleteBerryRocks.txt")
for(k in 1:10) usagecontent <- gsub("  ", " ", usagecontent)
cat(substring(usagecontent, 10), file=rdfile, append=TRUE)
# ARGUMENTS
cat(paste0("}\n\\arguments{\n"), file=rdfile, append=TRUE)
for(i in (anf+1):(end-1) )
  {
  # Split argument and explanation:
  arg_expl <- strsplit(rfile[i], "#")[[1]]
  # Remove leading and trailing white spaces:
  arg_expl <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", arg_expl, perl=TRUE)
  # remove trailing comma in Argument:
  if(grepl("[,]$", arg_expl[1]))
     arg_expl[1] <- substring(arg_expl[1], 1, nchar(arg_expl[1])-1)
  # split arg name and default value:
  if(grepl("=", arg_expl[1]))
     arg_expl[c(1,3)] <- strsplit(arg_expl[1], "=")[[1]]
  # Dots:
  if(grepl("...", arg_expl[1])) arg_expl[1] <- "\\dots"
  # Write to Rd-File:
  #browser()
  if( arg_expl[1] != ")" )
    {
    # write arg name and explanation
    cat(paste0("  \\item{",arg_expl[1],"}{",arg_expl[2]), file=rdfile, append=TRUE)
    # write default value:
    if(length(arg_expl)==3)
      cat(paste0(". DEFAULT: ", arg_expl[3]), file=rdfile, append=TRUE)
    #
    cat("}\n", file=rdfile, append=TRUE)
    }
  } # End Loop
# FOOTER
cat(paste0("}
\\details{}
\\value{}
\\section{Warning}{}
\\note{}
\\author{Berry Boessenkool, \\email{berry-b@gmx.de}, 2014}
\\references{}
\\seealso{\\code{\\link{help}} }
\\examples{
}
\\keyword{}
"), file=rdfile, append=TRUE)
setwd(owd)
} # End of Function

