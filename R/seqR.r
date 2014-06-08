# Add a range argument to seq
# Berry Boessenkool, berry-b@gmx.de, 2014-02-03

seqR <- function(
                 from=1,
                 to=1,
                 range,
                 ...)
{
# Input checking:
if(!is.vector(range)) stop("'range' must be a vector.")
if(!is.numeric(range)) stop("'range' must be numeric.")

# only set from and to if range is given as input:
if(!missing(range)) 
  {
  from <- range[1]     # first
  to <- tail(range,1)  # and last value
  if(length(range)>2)
     {
     from <- min(range, finite=TRUE) # min
     to   <- max(range, finite=TRUE) # and max
     }
  }
# now call seq with from and to (obtained from range)
seq(from=from, to=to, ...)
}
