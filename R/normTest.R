#' @title Test values for normality of distribution
#' @description Normality test: histogram with corresponding normal density distribution line, 
#' as well as p values for various normality tests.\cr
#' The package \code{nortest} is needed for full functionality. 
#' @return named vector of p values
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sep 2021
#' @seealso \code{\link{ks.test}}, \code{\link{shapiro.test}}
#' @keywords distribution
#' @export
#' @importFrom stats ks.test shapiro.test
#' @examples
#' normTest(rnorm(1000, mean=97, sd=8.9))
#' # if p > 0.05: accept Nullhypothesis that data are normally distributed.
#' normTest(rexp(30))
#' 
#' if(requireNamespace("pbapply")) replicate <- pbapply::pbreplicate
#' par(mfcol=c(7,6), mar=c(0,0.1,1,0.1), oma=c(2.5,1.5,2.5,0), las=1)
#' invisible(sapply(c("rnorm(10)", "rnorm(100)", 
#'                    "rexp(10)", "rexp(100)", 
#'                    "runif(10)", "runif(100)"), function(vv){
#'   check <- replicate(1e2, normTest(v=eval(str2lang(vv)), plot=FALSE))
#'   for(n in rownames(check)) 
#'    {hist(check[n,], breaks=seq(0,1,len=20), axes=FALSE, ylab="", xlab="", main="")
#'    if(n=="ShapiroWilk") title(main=vv, line=1, xpd=NA)
#'    if(vv=="rnorm(10)") title(ylab=n, line=0, xpd=NA)
#'    abline(v=0.05, col="blue", lwd=1, xpd=TRUE)
#'    }
#'   axis(1, at=0:1)
#'   }))
#' title(main="P values of tests for normality with", outer=TRUE, line=1.5)
#' 
#' @param v        Vector of values to be tested for normality
#' @param plot     Plot the histogram with the corresponding 
#'                 normal density distribution? DEFAULT: TRUE
#' @param main     Graph title. DEFAULT: \code{deparse(substitute(v))}
#' @param breaks   Number of bins. Exact, unlike in \code{\link{hist}}.
#'                 DEFAULT: 15
#' @param col      Color of bars. DEFAULT: "tan"
#' @param legend   Add legend text in topright? DEFAULT: TRUE
#' @param \dots    Further arguments passed to \code{\link{hist}}
#'
normTest <- function(
v, 
plot=TRUE,
main=deparse(substitute(v)), 
breaks=15,
col="tan",
legend=TRUE,
...)
{
force(main)
v <- na.omit(v)
n <- requireNamespace("nortest")
if(!n) warning("package 'nortest' not available, omitting those tests.")
mytry <- function(expr)
{
 res <- try(expr, silent=TRUE)
 if(!inherits(res, "try-error")) return(res)
 res <- sub("(v)","",res,fixed=TRUE)
 res <- sub("Error in nortest::", "", res)
 res <- trimws(res)
 warning("Ignoring ", res, call.=FALSE)
 return(NA)
}
# Output: p values
out <- c(
 ShapiroWilk       = mytry(shapiro.test(v)$p.value),
 KolmogorovSmirnov = mytry(ks.test(v, "pnorm", mean(v), sd(v))$p.value))
if(n) out <- c(out, 
 LillieforsKS     = mytry(nortest::lillie.test(v)$p.value),
 AndersonDarling  = mytry(nortest::ad.test(v)$p.value),
 CramerVonMises   = mytry(nortest::cvm.test(v)$p.value),
 PearsonChiSquare = mytry(nortest::pearson.test(v)$p.value),
 ShapiroFrancia   = mytry(nortest::sf.test(v)$p.value))
if(!plot) return(out)
# Graph:
hist(v, breaks=seqR(v, len=breaks), col=col, freq=FALSE, main=main, las=1, ...)
x <- seqR(par("usr")[1:2], length.out=200)
lines(x, dnorm(x, mean=mean(v), sd=sd(v)), col="red", lwd=3, xpd=TRUE)
if(legend)
  {
  legtext <- sort(out, decreasing=TRUE)
  legtext <- paste(names(legtext), round0(legtext,3,1))
  legtext <- paste(legtext, collapse="\n")
  text(par("usr")[2], par("usr")[4], legtext, adj=c(1,1))
  }
return(out)
}
