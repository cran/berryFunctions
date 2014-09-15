# Berry Boessenkool

shapeZoom <- function(
        shp,
        Time=3,
        steps=3,
        ...)
{
warning("shapeZoom will soon (Nov 2014) be moved from berryFunctions into a new package, probably on github.")
legend("top", "first klick topleft, then bottomright\nof area to zoom to",
           bty = "n", text.col = "orange")
w <- locator(2)
u <- par()$usr
X1 <- c(u[1] + (w$x[1] - u[1]) * 1:steps/steps)
X2 <- c(u[2] - (u[2] - w$x[2]) * 1:steps/steps)
Y1 <- c(u[3] + (w$y[2] - u[3]) * 1:steps/steps)
Y2 <- c(u[4] - (u[4] - w$y[1]) * 1:steps/steps)
for (i in 1:steps)
   {
   polygon(c(w$x, rev(w$x)), rep(w$y, each=2))
   Sys.sleep(Time/steps)
   plot(shp, xlim=c(X1[i],X2[i]), ylim=c(Y1[i],Y2[i]), yaxs="i", xaxs="i", ...)
   }
}
