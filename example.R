dyn.load("simple_range.so")

## make some ranges
beg.a <- (sort(sample(1:100, 5))
end.a <- (beg.a + sample(1:15, 5))

beg.b <- (sort(sample(1:100, 5)))
end.b <- (beg.b + sample(1:15, 5))

##visualise

plot.new()
par(mar=c(1,1,1,1))
plot.window(xlim=range( 0, max(c(end.a, end.b))), ylim=c(0,4))
y <- seq(0, 1.5, length.out=5)
rect( beg.a, y, end.a, y+0.3, col='blue' )
rect( beg.b, y+2, end.b, y+2.3, col='red' )
abline(v=c(beg.a, end.a))

olaps <- .Call('range_overlap', cbind(beg.a, end.a), cbind(beg.b, end.b))
