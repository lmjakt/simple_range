## requires dyn.load("simple_range.so"), but this can't be
## carried out here as the location is unknown.
## there is a way of obtaining the location of this file, but I forget
## how to do it at the moment. Should fix!

rangesOverlap <- function(a, b){
    if(!is.numeric(a) || !is.numeric(b))
        stop("both arguments must be numeric matrices");
    if(!is.double(a))
        a <- matrix(as.double(a), nrow=nrow(a), ncol=ncol(a))
    if(!is.double(b))
        b <- matrix(as.double(b), nrow=nrow(b), ncol=ncol(b))
    .Call( "range_overlap", a, b )
}

pointsInRanges <- function( ranges, points ){
    if(!is.numeric(ranges) || !is.numeric(points))
        stop("both arguments must be numeric matrices");
    if(!is.double(ranges))
        ranges <- matrix(as.double(ranges), nrow=nrow(ranges), ncol=ncol(ranges))
    if(!is.double(points))
        points <- as.double(points)
    .Call( "points_in_ranges", ranges, points )
}

