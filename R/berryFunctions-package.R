#' Berry's functions
#' 
#' Collection of functions, mainly connected with graphics and hydrology.\cr 
#' - zoom in X11 graphics\cr 
#' - plot rainfall-runoff data and optimize parameters for the unit hydrograph in the linear storage cascade\cr 
#' - write text to plots on top of colored fields in label size (halo-effect)\cr 
#' - draw scatterplots colored by 3rd dimension (as in image, which only deals with grids)\cr 
#' - draw histograms horizontally\cr 
#' - advancedly label date axes and logarithmic axes\cr 
#' - fit multiple functions (power, reciprocal, exponential, logarithmic, polynomial, rational) by regression\cr 
#' - convert lists to data.frames\cr
#' - and more...
#' 
#' @name berryFunctions-package
#' @aliases berryFunctions-package berryFunctions
#' @docType package
#' @note Get the most recent code updates at \url{https://github.com/brry}\cr
#' At some places you'll find \code{## not run} in the examples. These code
#' blocks were excluded from checking while building, mainly because they are
#' interactive and need mouseclicks, or because they open another device/file.
#' Normally, you should be able to run them in an interactive session. If you
#' do find unexecutable code, please tell me!\cr 
#' Feel free to suggest packages in which these functions would fit well.\cr 
#' I strongly depend on - and therefore welcome - any feedback!\cr\cr 
#' The following functions have been deprecated:\cr 
#' changeAttribute, showAttribute, shapeZoom: moved to \url{https://github.com/brry/shapeInteractive}\cr 
#' extremeStat, extremeStatLmom: moved to distLextreme in \url{https://github.com/brry/extremeStat}\cr 
#' compFiles has been renamed to \code{\link{compareFiles}}. combineTextfiles has been renamed to \code{\link{combineFiles}}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2011-2016
#' @keywords package documentation
#' @examples
#' 
#' # see   vignette("berryFunctions")
#' 
NULL
