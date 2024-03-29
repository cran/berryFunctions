#' expressions/values conditional on whether tested expression returns an error.
#' 
#' Does a given expression return an error? 
#' Return specific values/expressions for either case.
#' Useful for loops when you want to easily control values based on errors that arise.
#' 
#' @return Returns value or expression stated in \code{error_true} or \code{error_false},
#'  depending on whether the tested expression throws an error.
#' @author Nick Bultman, \email{njbultman74@@gmail.com}, September 2020
#' @seealso \code{\link{is.error}}
#' @keywords programming error
#' @export
#' @examples
#' if.error(  log(3),   "error", "no_error" )
#' if.error(  log(3),   "error", log(3)  )
#' if.error(  log(3),   log(6), "no_error" )
#' if.error(  log("a"), log(6), log(3) )
#' 
#' @param expr Expression to be tested for returning an error.
#' @param error_true Value or expression to be executed if tested expression returns an error.
#' @param error_false Value or expression to be executed if tested expression does not return an error.
#' 
if.error <- function(
  expr, 
  error_true, 
  error_false
){
if(is.error(expr)) error_true else error_false
}
