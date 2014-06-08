rmse <- function(
                a,
                b)
{
if(!(is.vector(a) & is.vector(b))) stop("input is not vectors")
if(length(a) != length(b)) stop("vectors not of equal length")
if(any(is.na(a)|is.na(b)))
   { warning("NAs were omitted")
   Na <- which(is.na(a)|is.na(b))
   a <- a[-Na] ; b <- b[-Na]
   } # end if NA
sqrt( sum((a-b)^2)/length(b) )
}
