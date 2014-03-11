SparseSignal <- structure(function
### Make a SparseSignal, which is a piecewise constant function
### defined on 1, ..., L. 
(first,
### Integer vector: first position of each segment.
 after,
### Integer vector: first position after each segment.
 value
### Numeric vector: segment values.
 ){
  stopifnot(is.numeric(first))
  stopifnot(is.numeric(after))
  stopifnot(is.numeric(value))
  stopifnot(first < after)
  stopifnot(c(first, after) > 0)
  ss <- data.frame(first=as.integer(first),
                   after=as.integer(after),
                   value)[value != 0,]
  class(ss) <- c("SparseSignal", "data.frame")
  ss
### data.frame with columns first, after, value. 1 row per segment,
### and there are no rows for segments with value 0.
}, ex=function(){
  ## SparseSignals do not have 0-valued segments.
  ss <- SparseSignal(c(10, 12, 15, 1000),
                     c(11, 13, 17, 1001),
                     c( 1, -2,  4,    0))
  print(ss)
})

