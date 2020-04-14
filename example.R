dyn.load("simple_range.so")

## make some ranges
beg.a <- as.double(sort(sample(1:100, 7)))
end.a <- as.double(beg.a + sample(1:15, 7))
## 
beg.b <- as.double(sort(sample(1:100, 7)))
end.b <- as.double(beg.b + sample(1:15, 7))

beg.c <- as.double(sample(1:100, 7))
end.c <- as.double(beg.a + sample(1:15, 7))


##visualise

plot.new()
par(mar=c(1,1,1,1))
plot.window(xlim=range( 0, max(c(end.a, end.b))), ylim=c(0,4))
y <- seq(0, 1.5, length.out=length(beg.a))
h <- diff(y)[1]
rect( beg.a, y, end.a, y+h, col='blue' )
rect( beg.b, y+2, end.b, y+2+h, col='red' )
abline(v=c(beg.a, end.a))

olaps <- .Call('range_overlap', cbind(beg.a, end.a), cbind(beg.b, end.b))

## and this gives us an error; 
olaps.2 <- .Call('range_overlap', cbind(beg.a, end.a), cbind(beg.c, end.c))
