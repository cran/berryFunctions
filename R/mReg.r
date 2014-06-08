### multiple Regression with several function types ----------------------------
# Berry Boessenkool, 12-2012, updated 04-2013 and 08-2013     berry-b@gmx.de
# Any feedback is welcome!

#  1 linear                 a*x + b
#  2 quadratic (parabola)   a*x^2 + b*x + c
#  3 kubic                  a*x^3 + b*x^2 + c*x + d
#  4 Polynom 4th degree     a*x^4 + b*x^3 + c*x^2 + d*x + e
#  5 Polynom 5              a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
#  6 logarithmic            a*log(x) + b
#  7 exponential            a*e^(b*x)
#  8 power/root             a*x^b
#  9 reciprocal             a/x + b
# 10 rational               1 / (a*x + b)
# 11 exponential 4 Param    a*e^(b*(x+c)) + d
# Yet to add: 12 hyperbolic             sinh(), cosh(), tanh()
# a^x + b

# ToDo's:
# - allow specification of initial values for optim
# - try exp_4 with nls
# - add hyperbolic functions
# - quantify / test influence of replacing zeros with near-zero values
# - bug hunt


mReg <- function(
    x,
    y, # x and y Data
    Poly45=FALSE, # Also fit polynomials of 4th and 5th degree?
    exp_4=FALSE, # return exp_4 fits in table? (only best fit is plotted) DEFAULT: FALSE
    xf=deparse(substitute(x)), yf=deparse(substitute(y)), # x and y names for Formula
    ncolumns=9, # number of columns in output. Set lower to avoid overcrowding the console
    plot=TRUE,   # plot data and fitted functions?
    add=FALSE,    #  add lines to existing plot?
    nbest=12, # number of best fitting functions to be plotted (console output table always has all)
    R2min, # minimum Rsquared value for function type to be plotted. Suggestion: 0.6 (2/3 of variation of y is explained by function of x)
    selection=NULL, # Integers of functions to be plotted, assigned as in list above.
    digits=2, # significant digits for rounding formula output and R^2 in legend
    extend=0.4, # extention of axis ranges (proportion of range)
    xlim=range(x, finite=TRUE) + c(-1,1)*extend*diff(range(x, finite=TRUE)), # default xlim
    ylim=range(y, finite=TRUE) + c(-1,1)*extend*diff(range(y, finite=TRUE)), # default ylim
    xlab=xf, #  default labels via substitute before replacing zeros in x and y
    ylab=yf, #
    las=1, # label axis style, see ?par
    lwd=rep(1,12), # some graphical parameters, all of length 12
    lty=rep(1,12),
    col=NULL, # 12 colors for lines and legend texts. DEFAULT: NULL, means they are specified internally
    pcol=par("col"), # color for the data points
    pch=16, #  point character for the data points
    legend=TRUE, #posx="top", posy=NULL, inset=0, # legend options
    legargs=NULL, # legend options
    legendform="nameform", # "full" formula, "form", "nameform" or only "name" in legend in plot
    ...) # more graphical parameters passed to plot
{ # Function start
# input checking
if( (xf %in% letters[1:6] | yf %in% letters[1:6])  &  legendform %in% c("nameform", "form")  )
   warning("Using single letters a to f for input variable names is not recommended, as formula forms will be difficult to read" )
if( xf=="e" | yf=="e" )
   warning("Using 'e' for input variable name is not recommended, as exponential formula forms will be difficult to read" )
if(any(4:5 %in% selection)) Poly45 <- TRUE
if(11 %in% selection) exp_4 <- TRUE
if( ! round(nbest,1) %in% 0:12) stop("nbest has to be an integer between 0 and 12")
if(length(lwd)==1) lwd <- rep(lwd, 12)
if(length(lty)==1) lty <- rep(lty, 12)
#
# Functions needed for function descriptions
# abbreviate parameters of fitted functions:
ab1 <- function(input) signif(input,digits)
# abbreviate parameters of fitted functions with algebraic sign (Vorzeichen):
ab <- function(input) paste0(ifelse(input>0, " + ", " - "),
                             abs(signif(input,digits)))
# Prepare Output Table
output <- as.data.frame(matrix(NA, ncol=if(Poly45) 9 else 7, nrow=10 ))
colnames(output) <- c("R2","Formulas","R2full", letters[1:(ncol(output)-3)] )
#
#
# To be removed if package is finished:
rsquare <- function(a,b) {
  if(any(is.na(a)|is.na(b)))
     {  Na <- which(is.na(a)|is.na(b)) ; a <- a[-Na] ; b <- b[-Na] }
  cor(a,b)^2
  }
rmse <- function(a,b) {  # Root Mean Square Error
  if(any(is.na(a)|is.na(b)))
     {  Na <- which(is.na(a)|is.na(b)) ; a <- a[-Na] ; b <- b[-Na] }
  sqrt( sum((a-b)^2)/length(b) )
  }
# Check whether always the functions from this package will be used...
# potentially replace this with
# rsquare <- berryFunctions:::rsquare
# rmse <- berryFunctions:::rmse
#
#
#  1 linear --------------- a*x + b --------------------------------------------
mod1 <- lm( y ~ x )
output$R2[1] <- summary(mod1)$r.squared
output$a [1] <- coef(mod1)[2]
output$b [1] <- coef(mod1)[1]
#  2 quadratic (parabola) - a*x^2 + b*x + c ------------------------------------
mod2 <- lm(y ~ I(x^2) + x)
output$R2[2] <- summary(mod2)$r.squared
output$a [2] <- coef(mod2)[2]
output$b [2] <- coef(mod2)[3]
output$c [2] <- coef(mod2)[1]
#  3 cubic ---------------- a*x^3 + b*x^2 + c*x + d ----------------------------
mod3 <- lm(y ~  poly(x,3, raw=TRUE))
output$R2[3] <- summary(mod3)$r.squared
output[3,4:7] <- rev(coef(mod3))
if(Poly45){
  #  4 Polynom4 ----------- a*x^4 + b*x^3 + c*x^2 + d*x + e --------------------
  mod4 <- lm(y ~  poly(x,4, raw=TRUE))
  output$R2[4] <- summary(mod4)$r.squared
  output[4, 4:8] <- rev(coef(mod4))
  #  5 Polynom5 ----------- a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f ------------
  mod5 <- lm(y ~  poly(x,5, raw=TRUE))
  output$R2[5] <- summary(mod5)$r.squared
  output[5,4:9] <- rev(coef(mod5))
  } # if Poly45 end
#  6 logarithmic ---------- a*log(x) + b ---------------------------------------
mod6 <- lm( y ~ log10(replace(x, x==0, 1e-10)) ) # influence needs yet to be checked! ###
output$R2[6] <- summary(mod6)$r.squared
output$a [6] <- coef(mod6)[2]
output$b [6] <- coef(mod6)[1]
#  7 exponential ---------- a*e^(b*x) ------------------------------------------
log_y <- log(replace(y, y==0, 1e-10))
mod7 <- lm( log_y ~ x )                       # y = a*e^(b*x)
output$R2[7] <- summary(mod7)$r.squared       # ln(y) = ln(a) + ln( e^(b*x) )
output$a [7] <- exp(coef(mod7)[1])            # ln(y) = ln(a) + b*x
output$b [7] <- coef(mod7)[2]
#  8 power/root ----------- a*x^b ----------------------------------------------
mod8 <- lm( log_y ~ log(replace(x, x==0, 1e-10)) ) # y = a*x^b
output$R2[8] <- summary(mod8)$r.squared            # ln(y) = ln(a) + ln(x^b)
output$a [8] <- exp(coef(mod8)[1])                 # ln(y) = ln(a) + b*ln(x)
output$b [8] <- coef(mod8)[2]
#  9 reciprocal ----------- a/x + b --------------------------------------------
mod9 <- lm( y ~ I(1/replace(x, x==0, 1e-10)) )
output$R2[9] <- summary(mod9)$r.squared
output$a [9] <- coef(mod9)[2]
output$b [9] <- coef(mod9)[1]
# 10 rational ------------- 1 / (a*x + b) --------------------------------------
mod10 <- lm( I(1/y) ~ x)
output$R2[10] <- summary(mod10)$r.squared
output$a [10] <- coef(mod10)[2]
output$b [10] <- coef(mod10)[1]
# 12 hyperbolic ----------- sinh(a*x+b)+c, cosh(), tanh() ----------------------
# yet to add
#
# name output rows -------------------------------------------------------------
rownames(output) <- c("linear", "square", "cubic", "poly4", "poly5",
     "logarithmic", "exponential", "power", "reciprocal", "rational" )#, "hyperbolic")
#
# 11 exp_4 ---------------- a*e^(b*(x+c))+d ------------------------------------
         # check this with nls: nls(y~expfun(p,x=x), start=param)    or  start=as.list(param)  ###
if(exp_4){  # 4-parametric exponential distibutions
  # initial parameters via lm of values relocated to first quadrant
  init_c <- -min(x, na.rm=TRUE)
  init_d <- min(y, na.rm=TRUE)
  y_11 <-  y - init_d  + 0.05*abs(init_d)   #; y_11[y_11==0] <- 0.001
  x_11 <-  x + init_c
  mod11 <- lm( log(y_11) ~ x_11 )
  init_a <- exp(coef(mod11)[1])
  init_b <- coef(mod11)[2]
  param <- c(a=init_a, b=init_b, c=init_c, d=init_d) ; names(param) <- letters[1:4]
  #
  # Exponential function to be fitted via optim
  expfun <- function(p, x) p["a"]*exp(p["b"]*(x+p["c"]))+p["d"]
  # function returning one value to be minimized via optim
  minfun <- function(p) rmse(y, expfun(p, x=x)) # Root Mean Square Error  (Function in the package BerryFunctions)
  # Fitting of parameters with different methods
  opt1 <-     optim(par=param, fn=minfun, method="Nelder-Mead")
  opt2 <-     optim(par=param, fn=minfun, method="BFGS")
  opt3 <-     optim(par=param, fn=minfun, method="CG")
  opt4 <- try(optim(par=param, fn=minfun, method="L-BFGS-B") , silent=TRUE)
  if(class(opt4)=="try-error") {opt4 <- try(optim(par=opt1$par, fn=minfun, method="L-BFGS-B") , silent=TRUE)
  if(class(opt4)=="try-error") {opt4 <- try(optim(par=opt2$par, fn=minfun, method="L-BFGS-B") , silent=TRUE)
  if(class(opt4)=="try-error") {opt4 <- try(optim(par=opt3$par, fn=minfun, method="L-BFGS-B") , silent=TRUE)
  if(class(opt4)=="try-error") {opt4 <- list(par=c(a=NA,b=NA,c=NA,d=NA))}}}}
  opt5 <-     optim(par=param, fn=minfun, method="SANN")
  optFits <- rbind(param, opt1$par, opt2$par, opt3$par, opt4$par, opt5$par)
  optR2 <- c(rsquare(y, expfun(param, x=x)),
          rsquare(y, expfun(opt1$par, x=x)),
          rsquare(y, expfun(opt2$par, x=x)),
          rsquare(y, expfun(opt3$par, x=x)),
          rsquare(y, expfun(opt4$par, x=x)),
          rsquare(y, expfun(opt5$par, x=x))    )
  optFits <- cbind(R2=optR2, Formulas=NA, R2full=NA, optFits)
  rownames(optFits) <- paste("exp_4par", c("ini", "N-M", "BFGS", "CG", "L--B", "SANN"), sep="_")
  if(Poly45) optFits <- cbind(optFits, data.frame(e=NA, f=NA))
    else optFits <- as.data.frame(optFits)
  optFits$Formulas <- paste0(yf," = ", ab1(optFits[,4]),"*e^(",
     ab1(optFits[,5]), "*(", xf, ab(optFits[,6]), "))", ab(optFits[,7]) )
  optFits <- optFits[ order(optFits[,1], decreasing=TRUE) , ]
  optFits$R2full <- optFits$R2
  optFits$R2 <- round(optFits$R2, digits)
  output <- rbind(output, optFits[1,]) # in output, for now only include best fit for plotting
  } # if exp_4 end
#
# Formulas of fitted functions -------------------------------------------------
output$Formulas[1] <- paste0(yf," = ", ab1(output$a[1]),"*",xf,      ab(output$b[1]) )
output$Formulas[2] <- paste0(yf," = ", ab1(output$a[2]),"*",xf,"^2", ab(output$b[2]),"*",xf,      ab(output$c[2]) )
output$Formulas[3] <- paste0(yf," = ", ab1(output$a[3]),"*",xf,"^3", ab(output$b[3]),"*",xf,"^2", ab(output$c[3]),"*",xf,      ab(output$d[3]) )
if(Poly45){
output$Formulas[4] <- paste0(yf," = ", ab1(output$a[4]),"*",xf,"^4", ab(output$b[4]),"*",xf,"^3", ab(output$c[4]),"*",xf,"^2", ab(output$d[4]),"*",xf,      ab(output$e[4]) )
output$Formulas[5] <- paste0(yf," = ", ab1(output$a[5]),"*",xf,"^5", ab(output$b[5]),"*",xf,"^4", ab(output$c[5]),"*",xf,"^3", ab(output$d[5]),"*",xf,"^2", ab(output$e[5]),"*",xf, ab(output$f[5]) )
} # end if Poly45
output$Formulas[6] <- paste0(yf," = ", ab1(output$a[6]),"*log10(",xf, ")", ab(output$b[6]) )
output$Formulas[7] <- paste0(yf," = ", ab1(output$a[7]),"*e^(",           ab1(output$b[7]), "*", xf, ")" )
output$Formulas[8] <- paste0(yf," = ", ab1(output$a[8]),"*", xf, "^",     ab1(output$b[8]) )
output$Formulas[9] <- paste0(yf," = ", ab1(output$a[9]),"/",xf,            ab(output$b[9]) )
output$Formulas[10]<- paste0(yf," = 1/( ", ab1(output$a[10]),              ab(output$b[10]), "*", xf, " )" )
#
# edit Rsquared columns in ouput table -----------------------------------------
output$R2full <- output$R2
output$R2 <- round(output$R2, digits)
ord <- order(output$R2full, decreasing=TRUE) # descending order of goodness of fit, for legend
#browser()
#
# plot data and functions ------------------------------------------------------
if(plot & nbest!=0) {
  if(!add)  plot(x, y, las=las, pch=pch, ylab=ylab, xlab=xlab, xlim=xlim, ylim=ylim, col=pcol, ...)
  # select function types that should be drawn:
  todraw <- rep(FALSE, 12)
  if(missing(R2min) & is.null(selection)) todraw[ord[1:nbest]] <- TRUE
  if(!is.null(selection)) todraw[selection] <- TRUE
  if(!missing(R2min)) todraw[output$R2full>=R2min] <- TRUE
  #
  xdraw <- seq(xlim[1], xlim[2], len=1000)
  xdrawtab <- data.frame(x=xdraw) # colnames(xdrawtab) <- as.character(xf) # not necessary, as poly uses "x" (the one in the function environment)
  if(is.null(col)) col <- c("black", "red", "green3", "chartreuse", "forestgreen", "blue", "cyan", "magenta", "yellow", "gray", "orange", "deeppink")
  #
  if(todraw[1]) lines(xdraw, predict( mod1 , xdrawtab ),          col=col[1], lwd=lwd[1], lty=lty[1]) # 1 linear
  if(todraw[2]) lines(xdraw, predict( mod2 , xdrawtab ),          col=col[2], lwd=lwd[2], lty=lty[2]) # 2 square
  if(todraw[3]) lines(xdraw, predict( mod3 , xdrawtab ),          col=col[3], lwd=lwd[3], lty=lty[3]) # 3 cubic
  if(Poly45){
  if(todraw[4]) lines(xdraw, predict( mod4 , xdrawtab ),          col=col[4], lwd=lwd[4], lty=lty[4]) # 4 polynomial 4th degree
  if(todraw[5]) lines(xdraw, predict( mod5 , xdrawtab ),          col=col[5], lwd=lwd[5], lty=lty[5]) # 5 polynomial 5th degree
  } # end if Poly45
  if(all(xdraw<=0)) warning("no logarithmic regression could be done, as there are no positive x values")
  xd2 <- xdraw[xdraw>0]
  if(todraw[6]) lines(xd2,   output$a[6]*log10(xd2)+output$b[6],  col=col[6], lwd=lwd[6], lty=lty[6]) # 6 logarithmic
  if(todraw[7]) lines(xdraw, output$a[7]*exp(output$b[7]*xdraw),  col=col[7], lwd=lwd[7], lty=lty[7]) # 7 exponential
  if(todraw[8]) lines(xdraw, output$a[8]*xdraw^output$b[8],       col=col[8], lwd=lwd[8], lty=lty[8]) # 8 power (Potenz)
  if(todraw[9]) lines(xdraw, output$a[9]/xdraw+output$b[9],       col=col[9], lwd=lwd[9], lty=lty[9]) # 9 reciprocal
  if(todraw[10])lines(xdraw, 1/(output$a[10]*xdraw+output$b[10]), col=col[10],lwd=lwd[10],lty=lty[10])# 10 rational
  if(todraw[11])lines(xdraw, output$a[11]*exp(output$b[11]*(xdraw+output$c[11]))+output$d[11], col=col[11],lwd=lwd[11], lty=lty[11]) # 11 exp_4par
  # if(todraw[12])lines(xdraw, output$a[12]*cosh(x)+output$b[12], col=col[12],lwd=lwd[12], lty=lty[12]) # 12 hyperbolic
# prepare and write legend -----------------------------------------------------
if(legend) {
  fForms <- c("a*x + b", "a*x^2 + b*x + c", "a*x^3 + b*x^2 + c*x + d",
     "a*x^4 + b*x^3 + c*x^2 + d*x + e", "a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f",
     "a*log(x) + b", "a*e^(b*x)", "a*x^b", "a/x + b", "1 / (a*x + b)", "a*e^(b*(x+c)) + d")
  #
  legendlabel <- if(legendform=="full") output$Formulas else
                 if(legendform=="form") fForms else
                 if(legendform=="nameform") paste(rownames(output), "  ", fForms) else
                 if(legendform=="name") rownames(output) else
                 stop("wrong legendform. Use 'full', 'form', 'nameform', or 'name'.")
  if(!Poly45) ord <- ord[-(length(ord)-0:1)] # remove last two (Poly4 and 5) from legend
  ord <- ord[ord %in% which(todraw) ] # keep only the ones that are to be plotted
  #
  legargdefaults <- list(x="top", bty="n", cex=0.8, text.col=col[ord],
       legend=paste(sprintf(paste0("%.",digits,"f"), output$R2), "  ", legendlabel)[ord])
  do.call(graphics::legend, args=owa(legargdefaults, legargs, "legend"))
  } # if legend end
  } # if plot end
#
# final output -----------------------------------------------------------------
if(exp_4) output <- rbind(output, optFits[-1,]) # best fit is already included
if(!Poly45) output <- output[-(4:5),] # remove excess rows
#
output <- output[order(output$R2full, decreasing=TRUE),]
#
if(ncolumns >9) ncolumns <- 9
if(ncolumns <0) ncolumns <- 0
if(ncolumns >7 & !Poly45) ncolumns <- 7
#
if(ncolumns !=0) return(output[,1:ncolumns])
#
} # Function end ---------------------------------------------------------------


