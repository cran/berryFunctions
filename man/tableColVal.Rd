\name{tableColVal}
\alias{tableColVal}
\title{Table with values with value-dependent colored backgrounds in pdf}
\description{Table with numbers and corresponding color in the background of each cell. (heatmap)}
\usage{tableColVal(mat, pdffile="table_col_val.pdf", pdf=FALSE, nameswidth=0.3,
namesheight=0.1, palette=rainbow(nrow(mat)*ncol(mat), start=0, end=0.7),
Range=range(mat,finite=TRUE), cex.titles=1, col.titles=1, cex.text=1, col.text=1,
main="code by\\nberry-b@gmx.de", cex.main=1, col.main=1, ...)}
\arguments{
  \item{mat}{Matrix with values}
  \item{pdffile}{Name of File to write to. DEFAULT: "table_col_val.pdf"}
  \item{pdf}{Should table be written to pdffile? (Else it will plot in x11). DEFAULT: FALSE}
  \item{nameswidth}{Relative width of row names at the left, as a percentage of plot. DEFAULT: 0.3}
  \item{namesheight}{Relative height of column names at the top. DEFAULT: 0.1}
  \item{palette}{Color palette for the heatmap. DEFAULT: rainbow(nrow(mat)*ncol(mat), start=0, end=0.7)}
  \item{Range}{Range of values mapped linearly to color palette. DEFAULT: range(mat,finite}
  \item{cex.titles}{Size of row and column labels. DEFAULT: 1}
  \item{col.titles}{Color of labels. DEFAULT: 1}
  \item{cex.text}{Size of cell text. DEFAULT: 1}
  \item{col.text}{Color of cell text. DEFAULT: 1}
  \item{main}{Title to be put in upper left cell. DEFAULT: "code by\\nberry-b@gmx.de"}
  \item{cex.main}{Title size. DEFAULT: 1}
  \item{col.main}{Title color. DEFAULT: 1}
  \item{\dots}{Further Arguments passed to \code{\link{pdf}}}
}
\value{None. PDF or plot produced.}
\author{Berry Boessenkool, \email{berry-b@gmx.de}, Nov. 2012}
\seealso{\code{\link{pdf}}, \code{\link{heatmap}}}
\examples{
Bsp <- matrix(c(21,23,26,27, 18,24,25,28, 14,17,23,23, 16,19,21,25), ncol=4, byrow=TRUE)
colnames(Bsp) <- paste0("Measure", LETTERS[1:4])
rownames(Bsp) <- paste("prod", 8:11, sep="_")
Bsp

tableColVal(Bsp, pdf=FALSE)
tableColVal(Bsp, pdf=FALSE, nameswidth=0.1) # relative to plot width
tableColVal(Bsp, pdf=FALSE, col.titles=2, cex.text=2, col.main=3)
tableColVal(Bsp, pdf=FALSE, Range=c(10,40))
tableColVal(Bsp, pdf=FALSE, palette=heat.colors(12))
tableColVal(Bsp, pdf=FALSE, palette=c(2,4,7), main="last\ncomparison")


\dontrun{tableColVal(Bsp)
dev.off()
tableColVal(Bsp, width=12, cex.text=2)  # further arguments to pdf possible.

Bsp2 <- matrix(sample(1:100, 30), ncol=6, byrow=TRUE)
graphics.off(); windows(height=4)
tableColVal(Bsp2, pdf=FALSE)
}
}
\keyword{hplot}
