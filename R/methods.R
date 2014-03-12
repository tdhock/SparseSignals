### Test if x is a SparseSignal.
is.SparseSignal <- function(x){
  "SparseSignal" %in% class(x)
### TRUE or FALSE.
}

### Add two SparseSignals.
"+.SparseSignal" <- structure(function(x, y){
  if(is.SparseSignal(y)){
    L <- addSparseSignals(x, y)
    with(L, SparseSignal(first, after, value))
  }else if(is.numeric(y)){
    if(length(y) %in% c(1, nrow(x))){
      x$value <- x$value + y
      x[x$value != 0,]
    }else{
      stop("only can add numeric vectors of size 1 or nrow(x)")
    }
  }else{
    stop("addition only defined when y is numeric or SparseSignal")
  }
}, ex=function(){
  ## adding two non-overlapping 1-segment signals gives a signal with
  ## 2 segments.
  x <- SparseSignal(1, 2, 5.5)
  x + SparseSignal(4, 10, 3.8) # 2 rows.
  ## adding two overlapping 1-segment signals with opposite values
  ## gives a completely sparse signal with 0 segments.
  none <- x + SparseSignal(1, 2, -5.5) # 0 rows.
  str(none)
  ## a more complicated example.
  x <- SparseSignal(c(10L, 12L, 14L),
                    c(11L, 13L, 16L),
                    c(  1,   2,   3))
  y <- SparseSignal(c(10L, 12L, 15L),
                    c(11L, 13L, 17L),
                    c( 1, -2,    4))
  x + y
  ## you can also just add vectors, but they need to be length 1 or
  ## the number of segments.
  x + 1000
  x + c(1, 200, 3000)
})

### Subtract two SparseSignals.
"-.SparseSignal" <- structure(function(x, y){
  if(is.SparseSignal(y)){
    y$value <- -y$value
    x + y
  }else if(is.numeric(y)){
    x + (-y)
  }else{
    stop("subtraction only defined when y is numeric or SparseSignal")
  }
}, ex=function(){
  ## subtracting two non-overlapping 1-segment signals gives a signal
  ## with 2 segments.
  x <- SparseSignal(1, 2, 5.5)
  x - SparseSignal(4, 10, 3.8) # 2 rows.
  ## subtracting a SparseSignal from itself gives an empty signal.
  none <- x - x
  str(none)
})

### Print a summary of the SparseSignal.
print.SparseSignal <- function(x, ...){
  nrows <- getOption("nrowSparseSignal")
  if(is.null(nrows))nrows <- 10
  cat(sprintf("SparseSignal with %d nonzero segments from %d to %d\n",
              nrow(x), x$first[1], x$after[nrow(x)]-1))
  print.data.frame(head(x, nrows))
  if(nrow(x) > nrows){
    cat("...\n")
  }
  invisible(x)
}

### Get a piecewise constant vector from first up to but not including
### after. This is inefficient and should be used only to compare
### speed with efficient operations.
getVector <- structure(function(ss, first, after){
  first <- as.integer(first)
  after <- as.integer(after)
  stopifnot(length(first)==1)
  stopifnot(length(after)==1)
  size <- after-first
  vec <- rep(NA, size)
  not.useful <- ss$after <= first & ss$first >= after
  useful <- ss[!not.useful,]
  vec.i <- 1 #next position to write to.
  ss.i <- first #next position in ss to read from.
  for(seg.i in 1:nrow(useful)){
    seg.first <- useful$first[seg.i]
    if(seg.first > ss.i){
      zero.size <- seg.first-ss.i
      vec.i.end <- vec.i+zero.size-1
      vec[vec.i:vec.i.end] <- 0
      vec.i <- vec.i.end+1
      ss.i <- seg.first
    }
    seg.after <- useful$after[seg.i]
    seg.size <- seg.after-seg.first
    vec.i.end <- vec.i+seg.size-1
    vec[vec.i:vec.i.end] <- useful$value[seg.i]
    vec.i <- vec.i.end+1
    ss.i <- seg.after
  }
  if(vec.i <= length(vec)){
    vec[vec.i:length(vec)] <- 0
  }
  vec
}, ex=function(){
  x <- SparseSignal(c(10, 15), c(13, 20), c(3.3, 5.5))
  show <- function(first, after){
    v <- getVector(x, first, after)
    data.frame(v, i=first:(after-1))
  }
  show(5, 25)
  show(10, 20)
  show(10, 21)
  show(9, 21)
})
