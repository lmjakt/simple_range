## requires dyn.load("simple_range.so")
## this performs some magic that should not be necessary if created
## as a package
dyn.load( paste(dirname(sys.frame(1)$ofile), "simple_range.so", sep="/") )
dyn.load( paste(dirname(sys.frame(1)$ofile), "arrange_lines.so", sep="/") )

rangesOverlap <- function(a, b){
    if(!is.numeric(a) || !is.numeric(b))
        stop("both arguments must be numeric matrices");
    if(!is.double(a))
        a <- matrix(as.double(a), nrow=nrow(a), ncol=ncol(a))
    if(!is.double(b))
        b <- matrix(as.double(b), nrow=nrow(b), ncol=ncol(b))
    .Call( "range_overlap", a, b )
}

## Returns the indices of:
## b: the last range where beg <= pos
## e: the first range where end >= pos
## for each position pos in in points
## Note: this will only report the first of a set of overlapping ranges
## that all contain the point.
## both points and ranges (by beg) must be sorted in ascending order.
pointsInRanges <- function( ranges, points ){
    if(!is.numeric(ranges) || !is.numeric(points))
        stop("both arguments must be numeric matrices");
    if(!is.double(ranges))
        ranges <- matrix(as.double(ranges), nrow=nrow(ranges), ncol=ncol(ranges))
    if(!is.double(points))
        points <- as.double(points)
    tmp <- .Call( "points_in_ranges", ranges, points )
    colnames(tmp) <- c("b", "e")
    tmp
}

## x should be a matrix with two columns
## if no columns with name x0 and x1, then the
## first two columns will be taken to represent x0 and x1
## respectively.
arrange.lines <- function(x){
    if(!is.matrix(x) || !is.numeric(x) || ncol(x) < 2)
        stop("x should be a numeric matrix with at least two columns")
    if(!all(c('x0', 'x1') %in% colnames(x)))
        colnames(x)[1:2] <- c('x0', 'x1')
    if(any( x[,'x0'] >= x[,'x1'] ))
        stop("all x0 values must be smaller than x1")
    if(any(is.na(x[,c('x0', 'x1')])))
       stop("NA values not allowed")
    .Call("arrange_lines", as.double(x[,'x0']), as.double(x[,'x1']))
}
