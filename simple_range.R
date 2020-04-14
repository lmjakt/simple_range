## requires dyn.load("simple_range.so"), but this can't be
## carried out here as the location is unknown.


rangesOverlap <- function(a, b){
    if(!is.numeric(a) || !is.numeric(b))
        stop("both arguments must be numeric matrices");
    if(!is.double(a))
        a <- matrix(as.double(a), nrow=nrow(a), ncol=ncol(a))
    if(!is.double(b))
        b <- matrix(as.double(b), nrow=nrow(b), ncol=ncol(b))
    do.Call( "range_overlap", a, b )
}
