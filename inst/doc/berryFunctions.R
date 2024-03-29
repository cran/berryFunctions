## ----instcran, eval=FALSE-----------------------------------------------------
#  install.packages("berryFunctions")
#  library(berryFunctions)

## ----instgit, eval=FALSE------------------------------------------------------
#  if(!requireNamespace("remotes", quitly=TRUE)) install.packages("remotes")
#  remotes::install_github("brry/berryFunctions", build_opts="--no-manual")

## ----library, echo=FALSE------------------------------------------------------
library(berryFunctions)

## ----colPoints, fig.show='hold', echo=-c(1:2)---------------------------------
par(mar=c(3.2,3.2,3,0.7), mgp=c(2.1,0.7,0))
set.seed(007)
x <- sample(1:87, 150, TRUE);   y <- sample(1:61, 150, TRUE);  z <- diag(volcano[x,y])-95
colPoints(x,y,z,  pch="+", legargs=list(y1=0.8,y2=1, title="Elevation  [m]"), add=FALSE)
mtext("colPoints, textField", outer=TRUE, adj=0.05, line=0.5, cex=1.2, font=2)

text(60,30, "unreadable text")
textField(60, 15, "good text", field="round", fill="orange", cex=1.2)

dat <- rbeta(1e4, 2, 80)*100; dat <- dat[dat>0.1]
logHist(dat, col="tan", breaks=50, main="logHist, logAxis")

## ----lsc, fig.show='hold', echo=-1, fig.height=3.5, fig.width=5.5, warning=FALSE----
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
# estimate parameters for Unit Hydrograph, plot data and simulation: lsc
QOBS <- dbeta(1:40/40, 3, 10) + rnorm(20,0,0.2) + c(seq(0,1,len=20), rep(1,20))
PREC <- c(1,1,3,4,5,5,4,3,1,1, rep(0,30))
lsc(PREC, QOBS, area=10, main="lsc, unitHydrograph, superPos") # , plotsim=F

## ----regression, fig.show='hold', echo=-(1:2)---------------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
options(digits=5)
a <- 1:30   ; b <- a/2.345+rnorm(30,0,3)
linReg(a,b, main="linReg, circle, addAlpha")

circle(12,3, r=5, col=addAlpha("darkgreen"), border="blue", lwd=3)

x <- c(1.3, 1.6, 2.1, 2.9, 4.4, 5.7, 6.6, 8.3, 8.6, 9.5)
y <- c(8.6, 7.9, 6.6, 5.6, 4.3, 3.7, 3.2, 2.5, 2.5, 2.2)
mReg(x,y, main="mReg")[,c(2,3,5:6)]

## ----tableColVal, echo=-1, fig.height=3.5, fig.width=5.5----------------------
par(mar=c(0,0,1,0))
tableColVal(as.matrix(eurodist)[1:15,1:5], nameswidth=0.25)

## ----climgraph, echo=-1, fig.height=3.5, fig.width=5.5------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
climateGraph(temp=c(-9.3,-8.2,-2.8,6.3,13.4,16.8,18.4,17,11.7,5.6,-1,-5.9),
             rain=c(46,46,36,30,31,21,26,57,76,85,59,46))

## ----df-----------------------------------------------------------------------
# Convert list with vectors of unequal length to one single data.frame: l2df
eglist <- list(AB=c(6,9,2,6), CD=1:8, EF=c(-3,2) )
eglist
l2df(eglist)  # names are even kept

# add rows to a data.frame: addRows, insertRows
MYDF <- data.frame(A=5:3, B=2:4)
addRows(MYDF, 3)
insertRows(MYDF, 2, 10:11)

# Order rows in a dataframe: sortDF
sortDF(USArrests[USArrests$Murder>14,], "Assault", decreasing=TRUE)

# truth table to test logical expressions: TFtest
TFtest(!a & !b, a&b, !(a&b))

# Head and tail at the same time: headtail (exception from lowerCamelCasing)  
headtail(iris, n=3, na=FALSE)

## ----showPal, fig.show='hold'-------------------------------------------------
showPal(cex=3)

neff <- t(replicate(n=300, sapply(1:200, function(nn) max(rnorm(nn)))   ))
qB <- quantileBands(neff, x=1:200, smooth=7)

## ----distrplots, fig.show='hold', echo=-1-------------------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
normPlot(mean=81.7, sd=11.45)
betaPlot(shape1=1.5, shape2=6) 

## ----betacomp, echo=-1, fig.height=4.5, fig.width=5.5-------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
betaPlotComp()

## ----lim0, fig.show='hold', echo=-1-------------------------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
val <- c(3.2, 1.8, 4.5, 8.2, 0.1, 2.9) # just some numbers
plot(val) # axes are extended by 4\% automatically, if xaxs="r"
plot(val, ylim=lim0(val), las=1) # you don't even have to set yaxs="i" ;-)

## ----horizHist, fig.show='hold', echo=-1--------------------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
ExampleData <- rnorm(200,13,5)
hpos <- horizHist(ExampleData, col=4)
abline(h=hpos(11), col=2, lwd=2)

groupHist(chickwts, "weight", "feed", col=2, unit="gr_6")
# drop the horsebean, feed those chicks with sunflower seeds (unless you like small chicken)

## ----pointZoom, eval=FALSE----------------------------------------------------
#  a <- rnorm(90); b <- rexp(90)
#  dev.new(record=TRUE) # turn recording on
#  plot(a,b, las=1)
#  pointZoom(a,b) # scroll through the plots (Pg Up and Pg Dn) to unzoom again.
#  
#  locLine()
#  
#  x <- rlnorm(700, m=3)
#  dev.new(record=TRUE) # scroll through the plots (Pg Up and Pg Dn)...
#  linLogHist(x, xlab="ddd", breaks=30, yaxt="n", freq=FALSE)

## ----movAvLines, fig.show='hold', echo=-(1:2), warning=FALSE------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
set.seed(42); a <- cumsum(rnorm(100))
plot(a, type="l", pch=16, las=1)
lines(movAv(a), col=2, lwd=3)
movAvLines(y=a, lwd=3)

X <- c(2, 224,  54,  505, 1,  5, 236,  92,  3, 0) # successful events
N <- c(2, 400, 100, 1000, 2, 10, 500, 200, 10, 2) # possible succeses
funnelPlot(X,N, letters[1:10])

## ----axes, fig.show='hold', echo=-(1:2)---------------------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
set.seed(42)
exdat <- 10^runif(50, -1, 2)
plot(exdat, log="y", yaxt="n")
logAxis(side=2) # invisibly returns values and labels
points(exdat, pch=16)

plot(as.Date("2013-04-25")+0:500, cumsum(rnorm(501)), type="l", xaxt="n", ann=FALSE)
dummy <- monthAxis(side=1)
str(dummy)

## ----hydro, echo=-1-----------------------------------------------------------
par(mar=c(3.2,3.2,1.5,0.7), mgp=c(2.1,0.7,0))
# superposition of precipitation to simulate Q from P: superPos
N <- c(9,5,2,14,1,3) # [mm/hour]
UH <- c(0.1, 0.4, 0.3, 0.1, 0.1) # [1/h]
superPos(N, UH)

# calculate continuous UH with given n and k: unitHydrograph
plot(0:40, unitHydrograph(n=2,  k=3, t=0:40), type="l")

# Nash-Sutcliffe and kling-gupta efficiency: nse + kge
QSIM <- lsc(PREC, QOBS, area=10, returnsim=TRUE, plot=FALSE)
nse(QOBS, QSIM)
kge(QOBS, QSIM)

# Root Mean Squared Error, e.g. to be minimized: rmse
rmse(QOBS, QSIM)

# R squared (coefficient of determination): rsquare
rsquare(QOBS, QSIM)

## ----trace--------------------------------------------------------------------
lower <- function(a, s) {tmessage("some stuff with ", a+10, skip=s); a}
upper <- function(b, skip=0) lower(b+5, skip)
upper(3) 

## ----tryStack-----------------------------------------------------------------
lower <- function(a) {message("fake message, a = ", a); a+10}
middle <- function(b) {plot(b, main=b) ; warning("fake warning, b = ", b); lower(b) }
upper <- function(c) {cat("printing c:", c, "\n") ; middle(c)}
tryStack(upper("42") )

## ----misc---------------------------------------------------------------------
# distance between two points on a plane: distance
A <- c(3,  9,-1)  ;  B <- c(7, -2, 4)
plot(A,B); points(3,5, col=2, pch=16); segments(3,5, A,B)
distance(A,B, 3,5)

# remove leading and trailing white space: removeSpace
s <- c("space at end     ", "  white at begin", "  both ", " special ^  ")
removeSpace(s)

# sequence given by range or vector of values: seqR
seqR(range=c(12,6), by=-2)
seqR(rnorm(20), len=7)

# Rescale values to another range: rescale
rescale(10:15, from=200, to=135)

# Show memory size of the biggest objects in MB: lsMem
lsMem(n=5)

# extract pdf link from google search result url: googleLink2pdf
Link <- paste0("http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1",
        "&cad=rja&sqi=2&ved=0CDIQFjAA&url=http%3A%2F%2Fcran.r-project.org",
        "%2Fdoc%2Fmanuals%2FR-intro.pdf&ei=Nyl4UfHeOIXCswa6pIC4CA",
        "&usg=AFQjCNGejDwPlor4togQZmQEQv72cK9z8A&bvm=bv.45580626,d.Yms")
googleLink2pdf(Link)

# Create a number of 999 strings with spaces for reading files: na9
na9()[c(1:4,13,30)]


## ----misc_non, eval=FALSE-----------------------------------------------------
#  # Separate lists with arguments for functions: owa
#  ?owa # the example section has a good - wait for it - example!
#  
#  # install.package and require in one single function: require2
#  require2(ada)
#  
#  # Write a file with a Roxygen-compatible function structure,
#  # making it easy to add new functions to the package: createFun
#  createFun(myNewFunction, package="extremeStat", path="S:/Dropbox")
#  
#  # Open the source code of a function on github: funSource
#  funSource("smoothLines")
#  
#  # Install a package from github without dependencies: instGit
#  instGit("brry/shapeInteractive")
#  
#  
#  # concatenate textfiles contents unchanged into one file: combineFiles
#  # see also: compareFiles, dupes
#  cat("This is Sparta.\nKicking your face.", file="BujakashaBerry1.txt")
#  cat("Chuck Norris will roundhousekick you.", file="BujakashaBerry2.txt")
#  combineFiles(inFiles=paste0("BujakashaBerry", 1:2, ".txt"),
#                   outFile="BujakashaBerry3.txt")
#  readLines("BujakashaBerry3.txt")
#  unlink(paste0("BujakashaBerry", 1:3, ".txt"))
#  
#  # wish neRds a happy new year: yearSample
#  yearSample(2016)
#  # Have a nerdy
#  set.seed(12353); sample(0:9,4,T)
#  
#  # generate name from "random" sample: nameSample
#  nameSample("berry")

## ----misc2--------------------------------------------------------------------
# Kind regards from
set.seed(8833277); paste(sample(letters,5,rep=T),collapse='')

