dyn.load("simple_range.so")
source("simple_range.R")

ranges <- matrix(nrow=20, ncol=2)
ranges[,1] <- sort( sample(1:10000, size=nrow(ranges)) )
ranges[,2] <- ranges[,1] + abs( rnorm( n=nrow(ranges), mean=400, sd=200 ) )


points <- sort(sample( 1:10000, size=20 ))

points.i <- pointsInRanges( ranges, points )

plot.new()
plot.window(xlim=c(1,11000), ylim=c(0,10))
rect(ranges[,1], 1, ranges[,2], 3 )
abline( v=points )

abline( v=points[ points.i[,1] == points.i[,2] & points.i[,1] <= length(points) ], col='red' )
