# argument overwriting
# combine default and user-specified argument lists
# Berry Boessenkool, late 2013 / early 2014

owa <- function(  # owa: overwrite arguments
       d,    #d: default
       a,    #a: arguments specified by user
       u=NA) #u: arguments that can not be overwritten (unchanged)
{
if(!is.null(a))
{
a <- a[ ! names(a) %in% u ] # discard arguments that should be left unchanged
d <- d[order(names(d))] # sort lists, so that order of args given in a is irrelevant
a <- a[order(names(a))]
d[names(d) %in% names(a)] <- a[names(a) %in% names(d)] # replace (overwrite)
result <- c(d, a[ !names(a) %in% names(d) ] ) # add further arguments given by the user, but not in the default
as.list(result)
}
else
as.list(d)
}

