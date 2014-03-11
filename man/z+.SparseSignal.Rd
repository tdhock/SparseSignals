\name{+.SparseSignal}
\alias{+.SparseSignal}
\title{+ SparseSignal}
\description{Add two SparseSignals.}
\usage{\method{+}{SparseSignal}(x, y)}
\arguments{
  \item{x}{
}
  \item{y}{
}
}



\author{Toby Dylan Hocking}




\examples{
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
}
