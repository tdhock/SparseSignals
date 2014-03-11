### Test if x is a SparseSignal.
is.SparseSignal <- function(x){
  "SparseSignal" %in% class(x)
### TRUE or FALSE.
}

### Add two SparseSignals.
"+.SparseSignal" <- function(x, y){
  if(is.SparseSignal(y)){
    .Call("add_SparseSignals_interface", x, y,
          package="SparseSignals")
    print("calling...")
  }else if(is.numeric(y)){
    if(length(y) %in% c(1, nrow(x))){
      x$value <- x$value + y
      x
    }else{
      stop("only can add numeric vectors of size 1 or nrow(x)")
    }
  }else{
    stop("addition only defined when y is numeric or SparseSignal")
  }
}
